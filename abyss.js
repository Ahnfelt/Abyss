function Vector(x, y) {
    return {x: x, y: y};
}

function addVectors(a, b) { 
    return Vector(a.x + b.x, a.y + b.y); 
}

function scaleVector(v, s) { 
    return Vector(v.x * s, v.y * s); 
}

function Entity(initial) {
    return {
        id: initial.id || (function() { throw "Property id is required to create an entity." })(),
    
        position: initial.position || Vector(0, 0),
        velocity: initial.velocity || Vector(0, 0),
        acceleration: initial.acceleration || Vector(0, 0),
        
        angle: initial.angle || 0,
        rotation: initial.rotation || 0,
        
        observed: {
            position: initial.position || Vector(0, 0),
            angle: initial.angle || 0,
        },
        
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

function updateEntity(entity, deltaSeconds) {
    var observedPosition = scaleVector(addVectors(scaleVector(entity.observed.position, 3), scaleVector(entity.position, 1)), 1/4);
    var observedAngle = entity.observed.angle; // TODO: How to take the average of angles
    return {
        id: entity.id,

        position: addVectors(entity.position, scaleVector(entity.velocity, deltaSeconds)),
        velocity: addVectors(entity.velocity, scaleVector(entity.acceleration, deltaSeconds)),
        acceleration: entity.acceleration,

        rotation: clampRadians(entity.rotation),
        angle: clampRadians(entity.angle + entity.rotation * deltaSeconds),

        observed: {
            position: addVectors(observedPosition, scaleVector(entity.velocity, deltaSeconds)),
            angle: clampRadians(observedAngle + entity.rotation * deltaSeconds),
        },

        state: entity.update != null ? entity.update(entity.state, deltaSeconds) : entity.state,
        update: entity.update,
        draw: entity.draw,
    };
}

function receive(event) {
    var input = JSON.parse(event.data);
    if(input[0] == "updateEntity") {
        var id = input[1];
        var entity = entities[id];
        entities[id] = $.extend(entity, input[2]);
    } else if(input[0] == "newEntity") {
        var id = input[1];
        var entity = Entity({
            id: "player",
            draw: function(entity, g) {
                g.translate(entity.observed.position.x, entity.observed.position.y);
                g.rotate(entity.observed.angle);
                g.drawImage(images.craft, -100, -50, 200, 100);
            },
        });
        entities[id] = $.extend(entity, input[2]);
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
    } else if(which == 67) {
        socket.send(JSON.stringify(["key", "shoot", pressed]));
        return true;
    }
    return false;
}

var entities = {};
var oldTime;
var context;
var socket;

function update() {
    var newTime = new Date().getTime();
    var deltaSeconds = (newTime - oldTime) / 1000;
    oldTime = newTime;
    var newEntities = {};
    for(id in entities) {
        newEntities[id] = updateEntity(entities[id], deltaSeconds);
    }
    entities = newEntities;
}

function draw(context) {
    context.save();
    context.clearRect(0, 0, canvasWidth, canvasHeight);
    for(id in entities) {
        var entity = entities[id];
        if(entity.draw != null) {
            context.save();
            entity.draw(entity, context);
            context.restore();
        }
    }
    context.restore();
}

function tick() {
    update();
    draw(foreground);
}

function initialize() {
    socket = new WebSocket('ws://192.168.2.100:8080');
    socket.onerror = function(event) { alert("Socket error: " + event); };
    socket.onclose = function(event) { alert("Socket closed: " + event); };
    socket.onmessage = receive;
    oldTime = new Date().getTime();
    setInterval(tick, 30);
}

