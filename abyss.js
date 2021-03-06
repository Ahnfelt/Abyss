$(function() {
    debugPanel = $("#debugInfo");
    debugPanel.hide();
    debug = Debug(debugPanel);
    debug.disable();
});

function Vector(x, y) {
    return {x: x, y: y};
}

function addVectors(a, b) { 
    return Vector(a.x + b.x, a.y + b.y); 
}

function scaleVector(v, s) { 
    return Vector(v.x * s, v.y * s); 
}

function Path(t0, a0, v0, p0) {
    return {
        t0: t0,
        a0: a0,
        v0: v0,
        p0: p0
    };
}

function getPosition(path, time) {
    return addVectors(addVectors(scaleVector(path.a0, (time - path.t0) * (time - path.t0)),  scaleVector(path.v0, (time - path.t0))), path.p0);
}

function getVelocity(path, time) {
    return addVectors(scaleVector(path.a0, 2 * (time - path.t0)),  path.v0);
}

function getAcceleration(path, time) {
    return path.a0;
}

function addPathsWeighted(path1, weight1, path2, weight2) {
    return Path(path1.t0,
            addVectors(scaleVector(path1.a0, weight1), scaleVector(getAcceleration(path2, path1.t0), weight2)),
            addVectors(scaleVector(path1.v0, weight1), scaleVector(getVelocity(path2, path1.t0), weight2)),
            addVectors(scaleVector(path1.p0, weight1), scaleVector(getPosition(path2, path1.t0), weight2)));
}

function Entity(initial) {
    var zeroPath = Path(0, Vector(0, 0), Vector(0, 0), Vector(0, 0));
    return {
        id: initial.id || (function() { throw "Property id is required to create an entity." })(),
        
        pendingPositionPaths: initial.pendingPositionPaths || [],
        newPositionPath: initial.newPositionPath || zeroPath,
        oldPositionPath: initial.oldPositionPath || zeroPath,
        crossfadeStart: initial.crossfadeStart || 0,

        angle: initial.angle || 0,
        rotation: initial.rotation || 0,
        
        state: initial.state || {},
        update: initial.update || null,
        draw: initial.draw || null
    };
}

function clampRadians(radians) {
    while(radians > 2 * Math.PI) {
        radians -= 2 * Math.PI;
    }
    while(radians < 0) {
        radians += 2 * Math.PI;
    }
    return radians;
}

function crossPath(entity, time) {
    var duration = entity.crossfadeStart - entity.newPositionPath.t0;
    if(time >= entity.crossfadeStart + duration) return entity.newPositionPath;
    var factor = sigmoid(time - entity.crossfadeStart, duration);
    return addPathsWeighted(entity.newPositionPath, factor, entity.oldPositionPath, 1 - factor);
}

function now() {
    return new Date().getTime() / 1000;
}

function receive(event) {
    var input = JSON.parse(event.data);
    if(input[0] == "welcome") {
        synchronizationTime = now();
        socket.send(JSON.stringify(["synchronize"]));
    } else if(input[0] == "time") {
        var rtt = now() - synchronizationTime;
        averageRoundTripTime = rtt;
        minRoundTripTime = rtt;
        receivedTime = input[1] + rtt / 2;
        receivedTimeOffset = now();
        ping();
    } else if(input[0] == "pong") {
        roundTripTime = now() - pingedTime;
        averageRoundTripTime = averageRoundTripTime * 4/5 + roundTripTime * 1/5;
        if(roundTripTime < minRoundTripTime) {
            minRoundTripTime = roundTripTime;
            receivedTime = input[1] + minRoundTripTime / 2;
            receivedTimeOffset = now();
        }
        setTimeout(ping, 2000);
    } else if(input[0] == "updateEntityPaths") {
        var id = input[1];
        var newPaths = input[2];
        entities[id] = $.extend(entities[id], {
            pendingPositionPaths: newPaths
        });
        debug.show(id + "-newPaths", newPaths)
    } else if(input[0] == "removeEntity") {
        var id = input[1];
        delete entities[id];
        debug.clear();
    } else if(input[0] == "newEntity") {
        var id = input[1];
        var entity = Entity({
            id: id,
            draw: function(entity, time, g) {
                var path = crossPath(entity, time);
                var position = getPosition(path, time);
                g.translate(position.x, -position.y);
                g.rotate(entity.angle);
                g.drawImage(images.craft, -100, -50, 200, 100);
            }
        });
        entities[id] = $.extend(entity, input[2]);
        debug.clear();
        debug.show(id + "-newPaths", input[2].pendingPaths)
    } else {
        alert("Error: did not understand message: " + event.data);
    }
}

function updateKey(which, pressed) {
    if(which == 37) {
        socket.send(JSON.stringify(["key", "left", pressed]));
        return true;
    } else if(which == 38) {
        socket.send(JSON.stringify(["key", "up", pressed]));
        return true;
    } else if(which == 39) {
        socket.send(JSON.stringify(["key", "right", pressed]));
        return true;
    } else if(which == 40) {
        socket.send(JSON.stringify(["key", "down", pressed]));
        return true;
    } else if(which == 32) {
        socket.send(JSON.stringify(["key", "shoot", pressed]));
        return true;
    } else if(which == 71) { // g
        if (pressed) {
            debugPanel.toggle("fast");
            debug.toggle();
        }
        return false;
    }
    return false;
}

function ping() {
    pingedTime = now();
    socket.send(JSON.stringify(["ping"]));
}

var entities = {};
var context;
var socket;
var synchronizationTime;
var pingedTime;
var roundTripTime;
var minRoundTripTime;
var averageRoundTripTime;
var receivedTime;
var receivedTimeOffset;

function update(time) {
    var newEntities = {};
    for(id in entities) {
        if(id.substring(0,6) != "entity") {
            debug.show(id, entities[id].newPositionPath);
        }
        var entity = entities[id];
        if(entity.pendingPositionPaths.length > 0) {
            var pendingPath = entity.pendingPositionPaths[0];
            if(time >= pendingPath.t0) {
                var oldPath = crossPath(entity, time);
                entity = $.extend(entity, {
                    pendingPositionPaths: entity.pendingPositionPaths.slice(1),
                    newPositionPath: pendingPath,
                    oldPositionPath: oldPath,
                    crossfadeStart: time
                });
            }
        }
        newEntities[id] = entity;
    }
    entities = newEntities;
}

function draw(context, time) {
    context.save();
    context.clearRect(0, 0, canvasWidth, canvasHeight);
    for(id in entities) {
        var entity = entities[id];
        if(entity.draw != null) {
            context.save();
            context.translate(canvasWidth / 2, canvasHeight / 2);
            entity.draw(entity, time, context);
            context.restore();
        }
    }
    context.restore();
}

function tick() {
    var currentTime = receivedTime + now() - receivedTimeOffset;
    update(currentTime);
    draw(foreground, currentTime);
    debug.show("Time", currentTime.toFixed(0) + "s");
    debug.show("Ping", (roundTripTime * 1000).toFixed(0) + "ms");
    debug.show("Ping avg", (averageRoundTripTime * 1000).toFixed(0) + "ms");
    debug.show("Entities", "" + objectLength(entities))
}

function initialize() {
    socket = new WebSocket('ws://localhost:8080');
    //socket = new WebSocket('ws://localhost:8080');
    socket.onerror = function(event) { alert("Socket error: " + event); };
    socket.onclose = function(event) { alert("Socket closed: " + event); };
    socket.onmessage = receive;
    receivedTime = 0;
    receivedTimeOffset = now();
    setInterval(tick, 10);
}

function sigmoid(t, rtt) {
    return 1 / (1 + Math.exp(-12 * (1/rtt) * t + 6));
}

function preferNewPathCrossfader(t, rtt) {
    return 0.5;
}

function objectLength(object) {
    var i = 0;
    for(var e in object) {
        if (object.hasOwnProperty(e)) i += 1;
    }
    return i;
}
