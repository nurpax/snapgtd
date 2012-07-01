
var Todo = Backbone.Model.extend({
    defaults: {
        descr: '',  // Some string
        completed: false
    },
    url:function() {
        return this.id ? "todo/"+this.id : "todo";
    }
});

var TodoList = Backbone.Collection.extend({
    model: Todo,
    url: "todo/list"
});

window.TodoItemView = Backbone.View.extend({
    tagName:"li",

    events: {
        "dblclick .view": "edit",
        "keypress .edit": "updateOnEnter"
    },

    template:_.template($('#tpl-todo-item').html()),

    initialize: function() {
        this.model.bind('change', this.render, this);
    },

    edit: function() {
        this.$el.addClass("editing");
        this.input.focus();
    },

    close: function() {
        var value = this.input.val();
        if (!value) this.clear();
        this.model.save({descr: value});
        this.$el.removeClass("editing");
    },

    updateOnEnter: function(e) {
        if (e.keyCode == 13) this.close();
    },

    render:function () {
        $(this.el).html(this.template(this.model.toJSON()));
        this.input = this.$('.edit');
        return this;
    }

});

window.TodoListView = Backbone.View.extend({
    initialize:function () {
        this.model.bind("reset", this.render, this);
    },

    render:function (eventName) {
        _.each(this.model.models, function (todo) {
            $(this.el).append(new TodoItemView({model:todo}).render().el);
        }, this);
        return this;
    }
});

var AppRouter = Backbone.Router.extend({
    routes:{
        "":"list",
    },

    list:function () {
        this.todoList = new TodoList();
        this.todoListView = new TodoListView({model:this.todoList});
        this.todoList.fetch();
        $('#todo-list').html(this.todoListView.render().el);
    },
});

var app = new AppRouter();
Backbone.history.start();
