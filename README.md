# Java Predicate Parser

This tiny library allows you to parse text representing a boolean expression at runtime.

## Getting the jar

### JitPack

You can add this library to your maven build using [JitPack](https://jitpack.io/).

Simply add the following to your `pom.xml`:

```
<repositories>
	<repository>
		<id>jitpack.io</id>
		<url>https://jitpack.io</url>
	</repository>
</repositories>
```
and
```
<dependencies>
	<dependency>
		<groupId>com.github.Sallatik</groupId>
		<artifactId>predicate-parser</artifactId>
		<version>1.0-RELEASE</version>
	</dependency>
</dependencies>
```

### Building from source

You will need Git and Maven installed in your system.

1. Clone the repo: `git clone https://github.com/Sallatik/predicate-parser.git`
2. Dive into the project root folder: `cd predicate-parser`
3. Build the project: `mvn package`
4. Find your jar inside of the `target` directory

## Documentation

To generate javadoc for this library, you will need to perform steps 1 and 2 from "Building from source" instruction.
After that, simply execute `mvn javadoc:javadoc`. You will find the documentation under the `target/site` directory.

## Usage

```
// define mappings for boolean operators

Map<String, Operators> operatorsMap = new HashMap<>();

operatorsMap.put("not", Operators.NOT);
operatorsMap.put("and", Operators.AND);
operatorsMap.put("or", Operators.OR);

// define mappings for the elementary predicates

Map<String, Predicate<Cat>> predicateMap = new HashMap<>();

predicateMap.put("fluffy", Cat::isFluffy);
predicateMap.put("black", cat -> cat.getColor() == Color.BLACK);
predicateMap.put("white", cat -> cat.getColor() == Color.WHITE);
predicateMap.put("hungry", Cat::isHungry);
predicateMap.put("tom", cat -> cat.equals(tom));

// Build a PredicateParser instance

PredicateParser<Cat> parser = SimplePredicateParser.<Cat>builder()
	.setCasePolicy(CasePolicy.TO_LOWER_CASE)
	.setOperatorMap(operatorMap)
	.setPredicateMap(predicateMap)
	.build();

// Use the parser to create predicates

Cat [] fluffyOrHungryColoredCatsExceptTom = getAllMyCats()
	.stream()
	.filter(parser.parse("(fluffy or hungry) and not (black or white or tom)")
	.toArray(Cat[]::new);
```
## Why???

Just in case you need to compile predicates at runtime.
I personally intend to use it in an annotation-based framework, but you can also use it for some querying mechanism or whatever.
