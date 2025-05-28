---
description: GSL Templating using Gemini 2.5 Pro
---

# Generator Scripting Language - teaching Gemini

During a recent course AI was explained and someone mentioned that
Gemini performs better then DeepSeek and ChatGPT in coding tasks.

So I took that opertunity to:

1. Teach Gemini about GSL ( www.github.com/zeromq/gsl)
2. Provide the README.txt file as context
3. Explain what I wanted.

## The assignment

I want to teach you about GSL, more context is provided here: [README.txt](https://github.com/zeromq/gsl/blob/master/README.txt).

Ask: Write an entities.xml file which contains some entities with typical fields such as name,age etc.

Gemini produced: 

### entities.xml

{% include "entities.xml" %}

After this I requested it to write the gsl-template to generate, on the basis of the entity-format C-structs.

### entity_generator.gsl
The resulting template after some back and forth looks like this:

{% include "entity_generator.gsl" %}

I then asked it to generate a main.c file and a Makefile in order to compile the entire example:

### main.c

{% include "entity_generator.gsl" %}

### Makefile

{% include "Makefile" %}



## Generated artifacts

As this should be a generic entity-generator, this first template generates C-code.
This might not be the cleanest code, however a 'seasoned' C-developer would be able
to provide a working version, which contains best practices for memory-management etc.

This working example could easily be taking by AI or a Human/AI - Centaur to generate
templates for other languages.

So, here come the generated artifiacts.

### Person.h

{% include "Person.h" %}

### Person.c

{% include "Person.c" %}
