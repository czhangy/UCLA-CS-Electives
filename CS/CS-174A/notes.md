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



## Lecture 2:  Output Devices

- Basic Elements (cont.)

  - Rendering
    - Visibility
    - Simulating light propagation
      - Reflection, absorption, scattering, emission, interference
    - Key elements
      - Camera, illumination, geometry/reflectivity
    - Draw visible surfaces onto display
      - Near plane to avoid division by 0 errors
      - Far plane to avoid unnecessary computation
      - Everything within the formed pyramid is the view frustrum
      - Everything outside is clipped out 
      - Scan converting is the process of determining which pixels to color when drawing
        - Issue: aliasing/pixelization
    - Textures
    - Non-Photorealistic Rendering
    - Information visualization
    - Resolving occlusions efficiently
    - Shading
      - Rasterization
  - Animation
    - Keyframe animation
      - Ex) Flipbook animation
    - Motion capture
    - Procedural animation
      - Ex) Physics-based animation, behavioral animation, emotion-based animation, etc.
    - Cloth simulation
    - Fluid simulation
      - Modeling
        - Incompressibility
        - Viscosity
      - Navier-Stokes equations
      - Level sets
    - Smoke simulation
      - Assumption: no viscosity
      - Rendering
        - Photon maps
        - Multiple scattering
    - Behavioral animation

- A Basic Graphics System

  - Input => CPU => GPU => frame-buffer => display

  - The GPU takes in a 3D world and generates a 2D image, with colors mapped to each pixel to form the image

    - Each pixel just needs to store a color
    - When the camera/objects/lighting is moved based on the interaction of the user, the image is recreated

  - Input Devices

    - Ex) Keyboard, mouse, controller, tablet & pen, other sensors (data glove, sound, gesture, etc.)

  - Output Devices

    - CRT (Cathode Ray-Tube)

      - Electrons strike phosphor coating and emits light
      - Direction of beam controlled by deflection plates
      - Random-scan, calligraphic, or vector CRT
      - Moving beam to new location
      - Refresh rate: 60Hz - 85Hz

    - Raster CRTs (`n x m` phosphor)

      - Frame-buffer depth

        - 1 bit: 2 levels only, black & white
        - 8 bits: grayscale, 256 gray levels or colors
        - 8 bits per color (RGB) = 24 bits = 16 million colors
        - 12 bits per color: HD

      - 3 different colored phosphors: triads

      - Shadow mask keeps the focus of the guns to a single triad, preventing neighboring pixels from blurring

      - Interlaced vs. non-interlaced displays

        - Non-interlaced means we go through each scan-line => large bandwidth requirement
        - Interlaced means we don't refresh each scan-line in every refresh
          - Odd/even split => cuts bandwidth requirement in half
        - Interlaced are used in commercial TV

      - Single vs. double buffering

        - The number of frame-buffers
        - Single frame-buffer can lead to problems like race conditions
        - In a double frame-buffer system, the GPU writes to one of the frame-buffers while the display reads from the other, swapping each frame
        - More reliability at the cost of extra memory size => memory is cheap

      - Screen Resolutions:

        - TV: 640x480
        - HD: 1920x1080
        - 4K LCD: 3840x2160
        - 35mm: 3000x2000

      - Memory & Space Requirements

        - Assuming:

          - Screen resolution = `n x m`
          - Refresh rate = `r` Hz (frames/second)
          - Color depth = `b` bits/pixel

        - $$
          \text{Memory space per second}=\frac{n\times m\times b\times r}{8}\text{ bytes}
          $$

        - If non-interlaced:

          - $$
            \text{Memory read time}=\frac{1}{n\times m\times r}\text{ secs/pixel}
            $$

        - If interlaced:

        - $$
          \text{Memory read time}=\frac{2}{n\times m\times r}\text{ secs/pixel}
          $$

    - Flat Screen Displays

      - Raster-based: active matrix with transistors at grid points
      - LEDs: light-emitting diodes
      - LCDs: polarization of liquid crystals
      - Plasma: energize gases to glow plasma

    - Other Output Devices

      - Printers & plotters: raster-based, no refresh
      - Stereo displays: 3D TVs/movies, fast switching of left and right eye polarized images

    - Virtual Reality

      - Flat panel technology
      - Stereoscopic
      - Track body, finger, and head locations
      - Foveated rendering: high resolution where viewer is focusing, low resolution elsewhere
      - Other input devices: force sensing gloves, sound, etc.



## Lecture 3:

- 



## Lecture 4:

- 



## Discussion 1: Introduction and Assignment 1

- Logistics
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
  - Exams
    - Freeform format, not multiple choice
    - Conduct as a normal in-person exam, paper-based
    - Math and theory, no actual coding
    - Maybe the most critical part if you want to get an A
    - Not curved
- Basic JavaScript
  - The world's most popular programming language
  - The programming language of the web
  - Easy to learn
  - Type system is very flexible, but sometimes too flexible
- Assignment 1
  - Purpose
    - Try WebGL and write the first CG webpage application by yourself
    - Get familiar with the developing environment for future assignments
  - Prerequisite
    - Very basic JS



## Discussion 2:

- 