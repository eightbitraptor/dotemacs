# Simple Ruby test file
class Person
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def greet
    puts "Hello, my name is #{@name} and I am #{@age} years old."
  end
end

person = Person.new("Alice", 30)
person.greet

# Test various Ruby syntax
numbers = [1, 2, 3, 4, 5]
doubled = numbers.map { |n| n * 2 }
puts doubled.inspect

# Hash example
options = {
  verbose: true,
  output: "result.txt",
  format: :json
}

options.each do |key, value|
  puts "#{key}: #{value}"
end