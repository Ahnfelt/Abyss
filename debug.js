function Debug(element) {
    return {
        stats: {},
        enabled: true,

        enable: function() {
            this.enabled = true;
        },

        disable: function() {
            this.enabled = false;
        },

        toggle: function() {
            this.enabled = ! this.enabled;
        },

        show: function(name, object) {
            this.stats[name] = object;
            this.updateStats();
        },

        remove: function(name) {
            delete this.stats[name];
            this.updateStats();
        },
        
        clear: function() {
            this.stats = {}
        },

        updateStats: function () {
            if (this.enabled) {
                element.empty();
                for (field in this.stats) {
                    element.append(this.buildNode(field, this.stats[field]));
                }
            }
        },

        buildTree: function (name, object) {
            var tree = $("<dl>");
            tree.append($("<dt>"+name+"</dt>"));
            for (var field in object) tree.append(this.buildNode(field, object[field]));
            return tree;
        },

        buildLeaf: function (name, value) {
            var valueString;
            if (typeof value == "number") {
                valueString = value.toFixed(2);
            } else {
                valueString = value;
            }
            return $("<span class=\"debugPropertyName\">"+name+"</span>: <span class=\"debugPropertyValue\">"+valueString+"</span>");
        },

        buildNode: function (name, data) {
            var container = $("<dd>");
            var content;
            if (typeof data == "object") {
                content = this.buildTree(name, data);
            } else {
                content = this.buildLeaf(name, data);
            }
            container.append(content);
            return container;
        }
    }
}

