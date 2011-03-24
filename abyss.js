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

function Entity(initial) {
    var zeroPath = Path(0, Vector(0, 0), Vector(0, 0), Vector(0, 0));
    return {
        id: initial.id || (function() { throw "Property id is required to create an entity." })(),
        
        positionPath: initial.positionPath || zeroPath,
        
        angle: initial.angle || 0,
        rotation: initial.rotation || 0,
        
        state: initial.state || {},
        update: initial.update || null,
        draw: initial.draw || null,
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

function receive(event) {
    var input = JSON.parse(event.data);
    if(input[0] == "keepAlive") {
        // Intentionally left empty
    } else if(input[0] == "pong") {
        roundTripTime = new Date().getTime() / 1000 - pingedTime;
        currentTime = input[1] + roundTripTime / 2;
        //setTimeout(ping, 1000);
    } else if(input[0] == "updateEntity") {
        var id = input[1];
        var entity = entities[id];
        entities[id] = $.extend(entity, input[2]);
    } else if(input[0] == "removeEntity") {
        var id = input[1];
        delete entities[id];
        debug.clear();
    } else if(input[0] == "newEntity") {
        var id = input[1];
        var entity = Entity({
            id: id,
            draw: function(entity, time, g) {
                var position = getPosition(entity.positionPath, time);
                g.translate(position.x, position.y);
                g.rotate(entity.angle);
                g.drawImage(images.craft, -100, -50, 200, 100);
            },
        });
        entities[id] = $.extend(entity, input[2]);
        debug.clear();
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
    pingedTime = new Date().getTime() / 1000;
    socket.send(JSON.stringify(["ping"]));
}

var entities = {};
var oldTime;
var context;
var socket;
var pingedTime;
var roundTripTime;
var currentTime = 0;

function update() {
    var newTime = new Date().getTime();
    var deltaSeconds = (newTime - oldTime) / 1000;
    currentTime += deltaSeconds;
    oldTime = newTime;
    var newEntities = {};
    for(id in entities) {
        debug.show(id, entities[id].positionPath);
        newEntities[id] = entities[id]; //updateEntity(entities[id], deltaSeconds);
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
            entity.draw(entity, time, context);
            context.restore();
        }
    }
    context.restore();
}

function tick() {
    update();
    draw(foreground, currentTime);
    debug.show("Time", currentTime);
    debug.show("Round-trip time", roundTripTime);
}

function initialize() {
    socket = new WebSocket('ws://mini.ahnfelt.dk:8080');
    socket.onerror = function(event) { alert("Socket error: " + event); };
    socket.onclose = function(event) { alert("Socket closed: " + event); };
    socket.onmessage = receive;
    oldTime = new Date().getTime();
    setTimeout(ping, 500); // To avoid a bug in Chrome?
    setInterval(tick, 10);
}


