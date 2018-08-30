#lang pollen
◊(require "utility.rkt")
◊input
◊input{i j k}
◊bit{i j}
◊input{◊input{◊input}}
◊input
◊input
◊$${◊input ◊input ◊key}
◊l{
- fuck
- that
- shit

boiiiiii...
Its bad for

ur health, ya fuck.

- What's happening here? A list starts anew...
◊i{This is an item too, and it deserves the love and respect that
every other item gets.}

- This is an item ◊i{and so is this.}
}
◊eql{
1
2 3
◊key ◊input ◊undefined{'one 'two}
4
}
◊set{1, 2, 3, 4, ◊key, ◊input, 5}
◊let-splice[()]{thing}
◊let-splice[()]{thing 1 and 2}
◊let-splice[([x 1] [y 2])]{x = ◊x, y = ◊y}
◊table{
    ◊row{
        1
        2
        3
        ◊i{4 5 6 trash}
    }
    ◊row{
        1
        2
        3
        ◊i{4 5 6 more mouth trash}
        7 8 9
        that last one is about cannibalism
        this is a ◊i{weird} line ◊i{many times over.}
    }
}
