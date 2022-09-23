# COM SCI 174A - Fall '22 - Law

[TOC]

## Lecture 1: Introduction

- Mathematics of Computer Graphics
  - Linear (vector/matrix) algebra
  - Coordinate systems
  - Geometry
    - Points, lines, planes, etc.
    - Curves, surfaces, etc.
  - Affine transformations
  - Projection transformations
- Computer Graphics
  - Art + mathematics + vision, optics + biomechanics + physics + engineering + artificial intelligence
- Applications of Computer Graphics
  - Entertainment
    - Films, games, virtual reality, etc.
  - Visualization
    - Scientific visualization, medical visualization, flight simulation, architecture, etc.
  - Education
  - etc.
- Basic Elements
  - Modeling
    - The actual modeling of objects in the scene
    - Simplest way to model an object is mathematically
      - Difficult to render when given in this form
    - Common practice is to break up the surface of the object into polygons
    - How do we model (mathematically represent) objects?
    - How do we construct models of specific objects?
    - Primitives:
      - 3D points
      - 3D lines and curves
      - Surfaces (BREPs): polygons, patches
        - Problem: you lose everything other than the surface of the model
      - Volumetric representations
      - Image-based representations
    - Attributes:
      - Color, texture maps
      - Lighting properties
    - Geometric transformations
      - How do you create a 2D photo from a 3D scene?
  - Animation
    - Moving objects in the scene, light sources, or the camera
    - How do we represent the motions of objects?
    - How do we give animators control of this motion?
  - Rendering
    - The main focus of the class
    - How do we simulate the real-world behavior of light
    - How do we simulate the formation of images?
  - Interaction
    - How do we enable humans and computers to interact?
    - How do we design human-computer interfaces?



## Lecture 2:

- 



## Discussion 1:

- Scope
  - Under the CS department
  - Needs a lot of a math for exams and assignments
    - Mainly linear algebra
    - You will learn theory about how shapes are rendered in computer graphics
  - Needs writing code for the assignments and team project
    - JavaScript (vanilla), GLSL
    - We use a framework called Tiny-Graphics
  - It is not about modeling and CG art, and we will not cover any of:
    - 3D model creation and rendering via industrial tools, like Maya, 3D Max, Blender
      - We will render 3D scenes directly from code and basic models/textures
    - Making games using game engines like UE or Unity
      - Of course, you can make a game as your team project, but you need to write all the game logic yourself
    - 3D digital art, architecture, movie post-effects, video editing, machine learning, AI, etc.
- Assignments
  - Assignment 1: Setting Up the Environment
  - Assignment 2: Basic Draw & Transformations
  - Assignment 3: Shading & Lighting
  - Assignment 4: Texture
- 