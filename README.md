# VSL
Very Simple Language

Reference: [crafting interpreters](https://craftinginterpreters.com/)

Example
```cpp
fun addn(x) {
  return fun (y) {
    return x + y;
  };
}

var addSix = addn(6);
print(addSix(3)); // 9

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
