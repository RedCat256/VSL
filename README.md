# VSL
Very Simple/Slow Language

A toy programing language for fun!

Simple: Lack of libraries.

Slow: A tree walking interpreter implemented in Racket.


Reference: [crafting interpreters](https://craftinginterpreters.com/)

Example


Closure
------
```cpp
fun addn(x) {
  return fun (y) {
    return x + y;
  };
}

var addSix = addn(6);
print(addSix(3)); // 9
```

Class
------
```cpp
class Dog {
  init(name, color) {
    this.name = name;
    this.color = color;
  }
  
  bark(n) {
    for (var i = 0; i < n; i = i+1)
      print("bark!");
  }
  
  tostr() {
    print(this.name);
    print(this.color);
  }
}

var dog = Dog("Putty", 3);
dog.bark(3); // bark!\nbark!\nbark!\n
dog.tostr(); // Putty\n3\n

```

List
------
```cpp
var lst = [1,2,3, "hello"];
for (var i = 0; i < 4; i = i+1)
  print(lst[i]);              // 1\n2\n3\nhello\n
```


