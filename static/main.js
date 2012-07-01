
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

    template:_.template($('#tpl-todo-item').html()),

    render:function (eventName) {
        $(this.el).html(this.template(this.model.toJSON()));
        return this;
    }

});

// Views
window.TodoListView = Backbone.View.extend({
    tagName:'ul',

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
        $('#todolist').html(this.todoListView.render().el);
    },
});

var app = new AppRouter();
Backbone.history.start();
