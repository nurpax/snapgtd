
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

$(function() {
    var f = new Todo();
    f.set({id:3});
    f.fetch({
        success: function() {
            console.log(f);
        }
    });

    var lst = new TodoList();
    lst.fetch({
        success: function() {
            console.log(lst);
        }
    });
});
