# SCAML - A purely functional Scala library for the YAML markup language

**_Note that this project is currently under heavy development and not near production ready._**

## Overview

### Features

* Pure YAML processor
* Generic derivation (TODO)
* Lenses integration (TODO)
* Speed (TODO)

## Quick-start

To add SCAML as a dependency to your Scala project:

```
sbt settings
```

Usage examples:

Example YAML:

```YAML
name: Thomas
address:
    street: Code Street
    number: 777
    addon: A
    postalCode:
      number: 20
      addon: ab
age: 200
```

Example Scala:

```scala
case class PostalCode(number: Int, addon: Option[String])

case class Address(street: String, number: Int, addon: Option[String], postalCode: PostalCode)

case class Person(age: Int, name: String, address: Address)

val file: String = File("example.json")

val person: Either[YamlError, Person] = YAML.read[Person](file)

println(Person.get)

val yamlObject: YamlObject = Yaml.write(person)

println(yamlObject)
```

## Documentation

Link to full documentation and usage guide.

## Contributing

All contributions, suggestions and feedback is welcome!

## License

Add license!