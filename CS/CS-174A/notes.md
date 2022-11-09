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



## Lecture 2: Output Devices

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



## Lecture 3: Vectors and Parametric Forms

- Review

  - A Basic Graphics System
    - Input devices: keyboard, mouse, tablet, touchscreens
    - CPU/GPU
    - Frame buffer, resolution, single vs. double buffering, color depth, interlaced vs. non-interlaced, refresh rate
    - Output devices: CRT (random-scan & raster), flat-panel (LED, LCD, plasma), printers, plotters, head-mounted devices, stereo displays
  - Linear Algebra
    - Vectors: magnitude, unit vector, normalizing, addition, multiplication, properties
    - Linear combination of vectors: affine, convex, linear independence

- A Basic Graphics System (cont.)

  - Output Devices
    - Anamorphic LED displays
    - Aliasing in Raster Displays
      - Aliasing
        - Lines
        - Polygons
      - Reasons for Aliasing
        - Location of pixels are fixed
        - Size of pixels are fixed

- Linear Algebra: The Algebra of Vectors and Matrices (and Scalars)

  - Vectors

    - `N`-tuple of scalar elements

    - $$
      \bold{v}=(x_1,x_2,...,x_n),\quad x_i\in\mathbb{R}
      $$

    - Magnitude:

      - $$
        |\bold{v}|=\sqrt{x_1^2+\ ...\ +x_n^2}
        $$

    - Unit Vectors:

      - 
        $$
        \bold{v}:|\bold{v}|=1
        $$

    - Normalizing a Vector:

      - $$
        \hat{\bold{v}}=\frac{\bold{v}}{|\bold{v}|}
        $$

    - Operations:

      - Addition:

        - $$
          \bold{x}+\bold{y}=(x_1+y_1,...,x_n+y_n)
          $$

      - Multiplication with Scalar (Scaling)

        - $$
          a\bold{x}=(ax_1,...,ax_n),\quad a\in\mathbb{R}
          $$

      - Properties:

        - $$
          \bold{u}+\bold{v}=\bold{v}+\bold{u}\\
          (\bold{u}+\bold{v})+\bold{w}=\bold{u}+(\bold{v}+\bold{w})\\
          a(\bold{u}+\bold{v})=a\bold{u}+a\bold{v},\quad a\in\mathbb{R}\\
          \bold{u}-\bold{u}=\bold{0}
          $$

      - Subtraction

        - Adding the negatively scaled vector

  - Linear Combination of Vectors

    - A linear combination of the `m` vectors `v1, ... , vm` is a vector of the form:

      - $$
        \bold{w}=a_1\bold{v}_1+\ ...\ +a_m\bold{v}_m,\quad a_1,...,a_m\in\mathbb{R{}}
        $$

    - The result of adding a point to a vector is a point

    - A vector can be defined, given a set of bases

  - Points

    - Points are reliant on a set of bases (2-bases for 2D space) and a point of reference/origin (where the bases join)

      - Bases must all be linearly independent

    - Points have a location, but do not have a direction, size, or shape

      - Vectors have size and direction, but do not have location

    - Subtracting two points produces a vector

      - $$
        \bold{v}=P_2-P_1
        $$

        - `P2` is the head (arrow-end), `P1` is the ta4il

    - Linear Combination of Points

      - The addition of two points is nonsensical

  - Parametric Form of a Line

    - $$
      P=(1-\alpha)P_1+\alpha P_2
      $$

      - When `α = 0`, `P = P1`
        - You are at the first endpoint of the line
      - When `α = 1`, `P = P2`
        - You are at the second endpoint of the line
      - As `α` increases from `0` to `1`, you move from the first endpoint to the second
      - We are scaling `P1` by `1 - α`, scaling `P2` by `α`, and then linearly combining them
      - When `α > 1`, we move beyond `P2`
      - When `α < 0`, we move beyond `P1`

    - In the context of computer graphics, we can figure out if we are on an edge by looking at `α`

      - We are on an edge if `0 ≤ α ≤ 1`
      - We can then use this to determine if two edges intersect

    - Generic Form:

      - $$
        P=\alpha_1P_1+\alpha_2P_2
        $$

        - Earlier, we constrained the problem so that `α1 + α2 = 1`, guaranteeing that `P` will be on the same line as `P1` and `P2` for any value of `α` => affine combination of points
        - An affine combination of points with the constraint `αi ≥ 0` means the point `P` is constrained to being on the edge => convex combination of points

  - Parametric Form of a Polygon

    - $$
      P=\alpha_1P_1+\ ...\ +\alpha_nP_n
      $$

      - Affine combination of points:

        - $$
          \sum_{i=1}^n\alpha_i=1
          $$

        - Guarantees that the points lie on the same plane as the polygon

      - Convex combination of points:

        - $$
          \sum_{i=1}^n\alpha_i=1\text{ and }\alpha_1,...,\alpha_n\ge0
          $$

        - The point `P` is guaranteed to lie in the same plane as and inside the points `P1, ... ,Pn`

  - Special Cases

    - Linear Combination

      - $$
        \bold{w}=a_1\bold{v}_1+\ ...\ +a_m\bold{v}_m,\quad a_1,...,a_m\in\mathbb{R}
        $$

    - Affine Combination

      - $$
        a_1+\ ...\ +a_m=1
        $$

    - Convex Combination

      - $$
        a_1+\ ...\ +a_m=1\text{ and }a_1,...,a_m\ge0
        $$

  - Linear Independence

    - For vectors `v1, ... , vm`:

      - $$
        a_1\bold{v}_1+\ ...\ +a_m\bold{v}_m=\bold{0}\text{ iff }a_1=\ ...\ a_m=0
        $$

  - Affine & Convex Combinations

    - Parametric Form of Lines
      - Line: infinite in both directions
      - Ray: infinite in one direction
      - Edge (or line segment): limited in both directions
      - Affine combination of points
      - Convex combination of points
    - Parametric Form of Planes
      - Affine combination of points
      - Convex combination of points

  - Generators and Base Vectors

    - How many vectors are needed to generate a vector space?
      - Any set of vectors that generate a vector space is called a generator set
      - Given a vector space `R^n` we can prove that we need minimum `n` vectors to generate all vectors `v` in `R^n`
      - A generator set with minimum size is called a basis for the given vector space

  - Standard Unit Vectors

    - The elements of a vector `v` in `R^n` are the scalar coefficients of the linear combination of the basis vectors

    - In 2D:

      - $$
        \bold{i}=(1,0)\\
        \bold{j}=(0,1)
        $$

    - In 3D:

      - $$
        \bold{i}=(1,0,0)\\
        \bold{j}=(0,1,0)\\
        \bold{k}=(0,0,1)
        $$

    - This course will generally use a right-hand coordinate system

## Lecture 4: Matrices, Homogeneous Representation, and Shapes

- Review:

  - Affine and Convex Combinations
    - Parametric Form of Lines
      - Line: infinite in both directions
      - Ray: infinite in one direction
      - Edge (or line segment): limited in both directions
      - Affine combination of points
      - Convex combination of points
    - Parametric Form of Places
      - Affine combination of points
      - Convex combination of points

- Vectors

  - Dot Products in Graphics

    - $$
      \bold{w},\bold{v}\in\mathbb{R}^n\\
      \bold{w}\cdot\bold{v}=\sum_{i=1}^nw_iv_i
      $$

    - The projection of `u` on `v`

    - Another problem dot products solve: comparing vectors

      - Trigonometry measurements

    - Properties:

      - Symmetry:

        - $$
          \bold{a}\cdot\bold{b}=\bold{b}\cdot\bold{a}
          $$

      - Linearity:

        - $$
          (\bold{a}+\bold{b})\cdot\bold{c}=\bold{a}\cdot\bold{c}+\bold{b}\cdot\bold{c}
          $$

      - Homogeneity:

        - $$
          (s\bold{a})\cdot\bold{b}=s(\bold{a}\cdot\bold{b})
          $$

      - $$
        |\bold{b}|^2=\bold{b}\cdot\bold{b}
        $$

      - $$
        \bold{a}\cdot\bold{b}=|\bold{a}||\bold{b}|\cos\theta
        $$

    - Dot Product and Perpendicularity

      - `a · b > 0` if the angle is acute
      - `a · b = 0` if the angle is right
      - `a · b < 0` if the angle is obtuse

    - Perpendicular Vectors

      - Vectors `a` and `b` are perpendicular iff `a · b = 0`

      - Also called "normal" or "orthogonal" vectors

      - It's easy to see that the standard unit vectors form an orthogonal basis:

        - $$
          \bold{i}\cdot\bold{j}=0\\
          \bold{j}\cdot\bold{k}=0\\
          \bold{i}\cdot\bold{k}=0
          $$

    - Projection

      - $$
        |\bold{u}|\cos\theta=\frac{\bold{u}\cdot\bold{v}}{|\bold{v}|}
        $$

      - Projection of vector `u` on unit vector `v`

      - Projection of vector `u` in `v`'s direction

    - The problems dot product solves:

      - Dotting with a vector of coefficients gives a linear function that maps a point onto a scalar
      - Predictable effect as you adjust a coordinate

  - Cross Product

    - Defined only for 3D vectors and with respect to the standard unit vectors

    - $$
      \bold{a}\times\bold{b}=(a_yb_z-a_zb_y)\bold{i}+(a_zb_x-a_xb_z)\bold{j}+(a_xb_y-a_yb_x)\bold{k}\\
      \bold{a}\times\bold{b}=\begin{vmatrix}
      \bold{i}&\bold{j}&\bold{k}\\
      a_x&a_y&a_z\\
      b_x&b_y&b_z
      \end{vmatrix}
      $$

    - Properties:

      - $$
        \bold{i}\times\bold{j}=\bold{k}\\
        \bold{i}\times\bold{k}=-\bold{j}\\
        \bold{j}\times\bold{k}=\bold{i}
        $$

      - Antisymmetry:

        - $$
          \bold{a}\times\bold{b}=-\bold{b}\times\bold{a}
          $$

      - Linearity:

        - $$
          \bold{a}\times(\bold{b}+\bold{c})=\bold{a}\times\bold{b}+\bold{a}\times\bold{c}
          $$

      - Homogeneity:

        - $$
          (s\bold{a})\times\bold{b}=s(\bold{a}\times\bold{b})
          $$

      - The cross product is normal to both vectors:

        - $$
          (\bold{a}\times\bold{b})\cdot\bold{a}=(\bold{a}\times\bold{b})\cdot\bold{b}=0
          $$

      - $$
        |\bold{a}\times\bold{b}|=|\bold{a}||\bold{b}|\sin\theta
        $$

- Matrices

  - Rectangular arrangement of scalar elements

  - Special Square Matrices

    - Zero matrix:

      - $$
        A_{ij}=0\text{ for all }i,j
        $$

    - Identity matrix:

      - $$
        I_n=\begin{cases}I_{ij}=1\text{ for all }i\\I_{ij}=0 \text{ for all }i\ne j\end{cases}
        $$

    - Symmetric matrix:

      - $$
        (A_{ij})=(A_{ji})
        $$

  - Operations:

    - Addition:

      - $$
        \bold{A}_{m\times n}+\bold{B}_{m\times n}=(a_{ij}+b_{ij})
        $$

      - Properties:

        - $$
          \bold{A}+\bold{B}=\bold{B}+\bold{A}\\
          \bold{A}+(\bold{B}+\bold{C})=(\bold{A}+\bold{B}+\bold{C})\\
          f(\bold{A}+\bold{B})=f\bold{A}+f\bold{B}
          $$

        - Transpose:

          - $$
            \bold{A}^T=(a_{ij})^T=(a_{ji})
            $$

    - Multiplication:

      - $$
        \bold{C}_{m\times r}=\bold{A}_{m\times n}\bold{B}_{n\times r}\\
        (\bold{C})_{ij}=\left(\sum_{k=1}^na_{ik}b_{kj}\right)
        $$

      - Properties:

        - $$
          \bold{AB}\ne\bold{BA}\\
          \bold{A}(\bold{BC})=(\bold{AB})\bold{C}\\
          f(\bold{AB})=(f\bold{A})\bold{B}\\
          \bold{A}(\bold{B}+\bold{C})=\bold{AB}+\bold{AC},(\bold{B}+\bold{C})\bold{A}=\bold{BA}+\bold{CA}\\
          (\bold{AB})^T=\bold{B}^T\bold{A}^T
          $$

  - Inverse of a Square Matrix

    - $$
      \bold{M}\bold{M}^{-1}=\bold{M}^{-1}\bold{M}=\bold{I}
      $$

    - $$
      (\bold{AB})^{-1}=\bold{B}^{-1}\bold{A}^{-1}
      $$

  - Dot Product as a Matrix Multiplication

    - $$
      \bold{a}\cdot\bold{b}=\bold{a}^T\bold{b}\\
      =\begin{pmatrix}a_1&a_2&a_3\end{pmatrix}\begin{pmatrix}b_1\\b_2\\b_3\end{pmatrix}\\
      =a_1b_1+a_2b_2+a_3b_3
      $$

- Next Up:

  - Homogeneous Representation of Points & Vectors
  - Spaces: Vector & Affine
  - Shapes
    - Lines, circles, polygons (triangles), polyhedrons
  - Transformations
    - Translation, scaling, rotation, shear
  - Spaces
    - Model space
    - Object/world space
    - Eye/camera space
    - Screen space

- Homogeneous Representation of Points & Vectors

  - Points vs. Vectors

    - What is the difference?
      - Points have location, but no size or direction
      - Vectors have size and direction, but no location
    - Problem: we represent 3D points and vectors both as 3-tuples

  - Homogeneous Representation

    - Convention: vectors and points are represented as 4x1 column matrices

    - 3D Homogeneous Vector:

      - $$
        \begin{bmatrix}v_1\\v_2\\v_3\\0\end{bmatrix}
        $$

    - 3D Homogeneous Point:

      - $$
        \begin{bmatrix}p_1\\p_2\\p_3\\1\end{bmatrix}
        $$

    - Normal to homogeneous:

      - For a vector, append `0` as a fourth coordinate
      - For a point, append `1` as a fourth coordinate

    - Homogeneous to normal:

      - Remove the fourth coordinate

  - Relationship between Points and Vectors

    - A difference between two points is a vector:

      - $$
        Q-P=\bold{v}
        $$

      - `v` is a vector from `P` to `Q`

    - We can consider a point as a base point plus a vector offset:

      - $$
        Q=P+\bold{v}
        $$

    - Holds in homogeneous form

- Spaces & Frames

  - Vector & Affine Spaces

    - Basis = vector space: support only vectors, not points
    - Frame = basis + point of reference/origin = affine space: support vectors and points
    - Basis defined by `v1, v2, v3`
    - Frame defined by `v1, v2, v3, P0`

  - Homogeneous Representation

    - Vectors & Points

      - $$
        \bold{v}=\beta_1v_1+\beta_2v_2+\beta_3v_3
        $$

      - $$
        P=P_0+\bold{v}=P_0+\alpha_1v_1+\alpha_2v_2+\alpha_3v_3
        $$

    - Linear Combination of Points

      - $$
        aP+bQ=a\begin{bmatrix}p_1\\p_2\\p_3\\1\end{bmatrix}+b\begin{bmatrix}q_1\\q_2\\q_3\\1\end{bmatrix}=\begin{bmatrix}ap_1+bq_1\\ap_2+bq_2\\ap_3+bq_3\\a+b\end{bmatrix}
        $$

      - What is it?

        - If `(a + b) = 0`, then it's a vector

        - If `(a + b) = 1`, then it's a point

        - Otherwise, ???

          - Any scalar multiple of a homogeneous point is the same homogenous point

          - $$
            \begin{bmatrix}2\\4\\6\\2\end{bmatrix}\equiv\begin{bmatrix}1\\2\\3\\1\end{bmatrix}
            $$

            - When the fourth value is `1`, the first three values are the Cartesian point

- Shapes

  - Discretization
    - We don't know how to tell a computer to draw most shapes because of their complicated, non-linear formulas
    - Instead, we linearize those shapes, breaking them up into a finite number of line segments between `N` discrete points
    - The more polygons you use, the smoother the surface, but the more expensive in space/time
  - Polygon
    - Collection of points connected with lines
    - Example:
      - Vertices: `v1, v2, v3, v4`
      - Edges: `e1 = v1v2, e2 = v2v3, e3 = v3v4, e4 = v4, v1`
    - Closed vs. open
    - Wireframe vs. filled
    - Planar vs. non-planar
    - Convex vs. concave
      - Concave: at least one interior angle is >180º
      - Two problems with concave polygons
        - Harder to detect points within the polygon's bounds
        - Harder to find the normal of the polygon
    - Simple vs. non-simple
  - Triangles
    - The most common primitive
    - Simple, convex, and planar



## Lecture 5: 2D Transformations

- Shapes

  - Polygonal Models/Data Structures
    - Indexed Face Set
      - Comprised of an indexed vertex list and faces
      - Use the index of vertices to define the faces of the shape
        - Vertices are listed in a counterclockwise manner to make it easier to calculate the outward normal

- Next Up:

  - Transformations
    - Translation, scaling, rotation, shear
    - Matrix representations
    - Inverse of these transformations
  - Spaces
    - Model space
    - Object/world space
    - Eye/camera space
    - Screen space

- Transformations

  - Translations (2D)

    - Geometrically:

      - Movement by some amount in each dimension

    - Arithmetically:

      - $$
        x'=x+t_x\\
        y'=y+t_y
        $$

      - $$
        \begin{bmatrix}x'\\y'\end{bmatrix}=\begin{bmatrix}t_x\\t_y\end{bmatrix}+\begin{bmatrix}x\\y\end{bmatrix}
        $$

  - Scaling (2D)

    - Geometrically:

      - Doesn't make sense to scale a point, only an object
      - Scale each vertex of the object by multiplying the coordinates of each vertex by some scaling factor, resulting in a resized object
        - Moves the object away from the origin if scaled by a factor `>1`
        - Moves the object towards the origin if scaled by a factor `<1`
        - A scaling factor of `1` does nothing in that dimension
        - A scaling factor of `-1` mirrors the object over the opposite axis
      - Scaling happens *with respect to the origin*
      - Uniform scaling: when `Sx = Sy`

    - Arithmetically:

      - $$
        x'=S_xx\\
        y'=S_yy
        $$

      - $$
        \begin{bmatrix}x'\\y'\end{bmatrix}=\begin{bmatrix}S_x&0\\0&S_y\end{bmatrix}\begin{bmatrix}x\\y\end{bmatrix}
        $$

  - Rotation (2D)

    - Geometrically:

      - Rotation of a point about the origin by some angle `θ`, counterclockwise
      - The distance of the point from the origin is `r`
      - The angle between the original point and the x-axis is `Φ`

    - Arithmetically:

      - $$
        x=r\cos\phi\quad y=r\sin\phi\\
        \begin{align*}
        x'&=r\cos(\phi+\theta)\\
        &=r\cos\phi\cos\theta-r\sin\phi\sin\theta\\
        &=x\cos\theta-y\sin\theta\\
        y'&=r\sin(\phi+\theta)\\
        &=r\sin\phi\cos\theta+r\cos\phi\sin\theta\\
        &=x\sin\theta+y\cos\theta
        \end{align*}
        $$

      - $$
        \begin{bmatrix}x'\\y'\end{bmatrix}=\begin{bmatrix}\cos\theta&-\sin\theta\\\sin\theta&\cos\theta\end{bmatrix}\begin{bmatrix}x\\y\end{bmatrix}
        $$

  - Shear (2D)

    - Geometrically:

      - Deformation to an object caused by a shearing factor `Sh(a)` in a direction
      - Coordinates in the other direction don't change

    - Arithmetically

      - In `x` direction:

        - $$
          x'=x+ay\\
          y'=y
          $$

        - $$
          \begin{bmatrix}
          x'\\
          y'
          \end{bmatrix}
          \begin{bmatrix}
          1&a\\
          0&1
          \end{bmatrix}
          \begin{bmatrix}
          x\\
          y
          \end{bmatrix}
          $$

      - In `y` direction:

        - $$
          x'=x\\
          y'=bx+y
          $$

        - $$
          \begin{bmatrix}
          x'\\
          y'\\
          \end{bmatrix}
          \begin{bmatrix}
          1&0\\
          b&1\\
          \end{bmatrix}
          \begin{bmatrix}
          x\\
          y\\
          \end{bmatrix}
          $$

  - Homogeneous Matrices

    - Given any sequence of transformations from right to left, we can use consecutive matrix multiplications to produce a single transformation matrix, which can then be applied to each point of an object

      - We must expand our matrix arithmetic to 3x3 in order to match our homogeneous matrices

    - Translation

      - $$
        \begin{bmatrix}
        x'\\
        y'\\
        w'
        \end{bmatrix}=
        \begin{bmatrix}
        1&0&t_x\\
        0&1&t_y\\
        0&0&1
        \end{bmatrix}
        \begin{bmatrix}
        x\\
        y\\
        1
        \end{bmatrix}
        $$

    - Scaling

      - $$
        \begin{bmatrix}
        x'\\
        y'\\
        w'
        \end{bmatrix}=
        \begin{bmatrix}
        S_x&0&0\\
        0&S_y&0\\
        0&0&1
        \end{bmatrix}
        \begin{bmatrix}
        x\\
        y\\
        1
        \end{bmatrix}
        $$

    - Rotation

      - $$
        \begin{bmatrix}
        x'\\
        y'\\
        w'
        \end{bmatrix}=
        \begin{bmatrix}
        \cos\theta&-\sin\theta&0\\
        \sin\theta&\cos\theta&0\\
        0&0&1
        \end{bmatrix}
        \begin{bmatrix}
        x\\
        y\\
        1
        \end{bmatrix}
        $$

    - Shear

      - In `x` direction:

        - $$
          \begin{bmatrix}
          x'\\
          y'\\
          w'
          \end{bmatrix}
          \begin{bmatrix}
          1&a&0\\
          0&1&0\\
          0&0&1
          \end{bmatrix}
          \begin{bmatrix}
          x\\
          y\\
          1
          \end{bmatrix}
          $$

      - In `y` direction:

        - $$
          \begin{bmatrix}
          x'\\
          y'\\
          w'
          \end{bmatrix}
          \begin{bmatrix}
          1&0&0\\
          b&1&0\\
          0&0&1
          \end{bmatrix}
          \begin{bmatrix}
          x\\
          y\\
          1
          \end{bmatrix}
          $$

  - Inverses

    - Translation

      - $$
        T^{-1}(t_x,t_y)=T(-t_x,-t_y)
        $$

    - Scaling

      - $$
        S^{-1}(S_x,S_y)=S\left(\frac{1}{S_x},\frac{1}{S_y}\right)
        $$

    - Rotation

      - $$
        R^{-1}(\theta)=R(-\theta)
        $$

    - Shear

      - $$
        Sh_x^{-1}(a)=Sh_x(-a)\\
        Sh_y^{-1}(b)=Sh_y(-b)
        $$



## Lecture 6: 3D Transformations

- Last Lecture Recap

  - Polygons (triangles)
  - Transformations: translation, scaling, rotation, shear
    - Geometrical representation
    - Mathematical representation
    - Homogeneous representation
  - Inverse of Transformations

- Next Up

  - Concatenation of transformations
  - Spaces: model, object/world, eye/camera, screen
  - Projections: parallel and perspectives
  - Midterm
  - Lighting
  - Flat and Smooth Shading

- Rendering Pipeline

  - Model Space => Transformation Matrix => World Space => Eye Matrix => Eye Space => Projection Matrix => Projection Space => Window-To-Viewport Mapping => Screen Space

- Extending 2D Transformations to 3D

  - Translation

    - $$
      \begin{bmatrix}
      1&0&0&t_x\\
      0&1&0&t_y\\
      0&0&1&t_z\\
      0&0&0&1
      \end{bmatrix}
      $$

      - The upper-left 3x3 is an orthonormal matrix => each row is a unit vector and all rows are perpendicular to every other row
      - Rigid body transformation => the lengths and angles within the body do not change
        - Connected to the nature of their orthonormal matrix
        - Rigid body transformation <=> orthonormal matrix

  - Scaling

    - $$
      \begin{bmatrix}
      S_x&0&0&0\\
      0&S_y&0&0\\
      0&0&S_z&0\\
      0&0&0&1
      \end{bmatrix}
      $$

      - The upper-left 3x3 is an orthogonal matrix => all rows are perpendicular to every other row, but each row is not a unit vector
      - 

  - Rotation

    - $$
      R_z(\theta)=\begin{bmatrix}
      \cos\theta&-\sin\theta&0&0\\
      \sin\theta&\cos\theta&0&0\\
      0&0&1&0\\
      0&0&0&1
      \end{bmatrix}
      $$

    - $$
      R_x(\theta)=\begin{bmatrix}
      1&0&0&0\\
      0&\cos\theta&-\sin\theta&0\\
      0&\sin\theta&\cos\theta&0\\
      0&0&0&1
      \end{bmatrix}
      $$

    - $$
      R_y(\theta)=\begin{bmatrix}
      \cos\theta&0&\sin\theta&0\\
      0&1&0&0\\
      -\sin\theta&0&\cos\theta&0\\
      0&0&0&1
      \end{bmatrix}
      $$

      - The upper-left 3x3 on each matrix is also an orthonormal matrix
        - Also a rigid body transformation

  - Shear

    - $$
      \begin{bmatrix}
      1&0&SH_x&0\\
      0&1&SH_y&0\\
      0&0&1&0\\
      0&0&0&1
      \end{bmatrix}
      $$

- Affine Transitions

  - All transformations we've looked at are affine
  - Properties:
    - Maintains linearity => if we're transforming a straight line, it will remain a straight line after transformation
    - Collinearity => if we have three points that are collinear before transformation, they will remain collinear after transformation
    - Parallelism => if we have two edges that are parallel before transformation, they will remain parallel after transformation
    - Planarity => if we have a set of points that are coplanar, they will remain coplanar after transformation
    - Ratio of edge lengths => relative ratios of edges are preserved through transformation
  - Rigid body transformation is a special case of affine transformation

- Concatenation of Transformations

  - In general, we know that matrix multiplication is not commutative
  - Affine transformations are commutative and additive with themselves
  - Examples:
    - Rotation followed by translation vs. translation followed by rotation
    - Rotation about a random point (not the origin)
    - Rotation about a random axis

- More on Orthonormal Matrices

  - $$
    R_z^{-1}(\theta)=R_z(-\theta)=\begin{bmatrix}
    \cos(-\theta)&-\sin(-\theta)&0&0\\
    \sin(-\theta)&\cos(-\theta)&0&0\\
    0&0&1&0\\
    0&0&0&1
    \end{bmatrix}=\begin{bmatrix}
    \cos\theta&\sin\theta&0&0\\
    -\sin\theta&\cos\theta&0&0\\
    0&0&1&0\\
    0&0&0&1
    \end{bmatrix}
    $$

    - Note that, for orthonormal matrices:

      - $$
        M^{-1}=M^T
        $$

      - Made up of translations and rotations only



## Lecture 7: Transformation Composition and Eye Space

- Last Lecture Recap

  - Examples of Transformations
    - Translation, scaling, rotation, shear

- Next Up

  - Concatenation of Transformations (cont.)
  - Spaces:
    - Model space
    - Object/world space
    - Eye/camera space
    - Screen space
  - Projections: parallel and perspective
  - Midterm
  - Lighting, flat/smooth shading

- Examples of Transformation Composition (cont.)

  - Rotation about a random axis

    - $$
      \cos\theta=\frac{u_x}{\sqrt{u_x^2+u_z^2}},\quad\sin\theta=\frac{u_z}{\sqrt{u_x^2+u_z^2}}
      $$

    - $$
      \cos\phi=\frac{\sqrt{u_x^2+u_z^2}}{|\bold{u}|},\quad\sin\phi=\frac{u_y}{|\bold{u}|}
      $$

    - $$
      M=T(O')R_y(-\theta)R_z(-\phi)R_x(\alpha)R_z(\phi)R_y(\theta)T(-O')
      $$

      - Move axis to origin, align axis with x-y plane, align axis with x-axis, perform required rotation about x-axis, then reverse the other transformations

  - Transforming a vector/normal

    - For vertices/points: `M`
    - For vectors (normals): `(M^T)^-1`
      - `(M^T)^-1 = M` for orthonormal matrices

- Transformations of Coordinate Systems

  - General Rotation Matrix

    - $$
      \begin{bmatrix}
      x'\\
      y'\\
      z'\\
      w'
      \end{bmatrix}=\begin{bmatrix}
      i_x'&j_x'&k_x'&0\\
      i_y'&j_y'&k_y'&0\\
      i_z'&j_z'&k_z'&0\\
      0&0&0&1
      \end{bmatrix}\begin{bmatrix}
      x\\
      y\\
      z\\
      1
      \end{bmatrix}
      $$

  - Coordinate systems consist of basis vectors and an origin (point)

    - They can be represented as affine matrices
    - Therefore, we can transform them just like points and vectors
    - This provides an alternate way to think of transformations: as changes of coordinate systems

- Eye Space

  - $$
    \bold{k}=\bold{E},\quad\bold{i}=\bold{Top}\times\bold{E},\quad\bold{j}=\bold{k}\times{\bold{i}}
    $$

  - $$
    EM=[\text{Mirror}_x][\text{GRM}]T(-\text{Loc})
    $$

    - $$
      \text{Mirror}_x=\begin{bmatrix}
      -1&0&0&0\\
      0&1&0&0\\
      0&0&1&0\\
      0&0&0&1
      \end{bmatrix}
      $$

- 



## Lecture 8: Projection Space

- Recap:

  - Transformation Matrix

    - $$
      RS_cShT=TM
      $$

    -  Unique for each object in the scene

  - Eye Matrix

    - $$
      [\text{Mirror}_x][\text{GRM}]T(-\text{Eye})=EM
      $$

    - The same for each object in the scene

- Composite 3D Rotation About the Origin

  - $$
    R(\theta_1,\theta_2,\theta_3)-R_z(\theta_3)R_y(\theta_2)R_x(\theta_1)
    $$

  - This is known as the "Euler angle" representation of 3D rotations

  - The order of rotation matrices is important

  - Note: the Euler angle representation suffers from sigularities

- Normals in Graphics

  - Finding a Normal

    - May be given to us in an indexed list, as either the normals of the vertices or the normals of the faces

    - Given a plane equation:

      - $$
        ax+by+cz=d
        $$

      - The normal of the plane is:

        - 

        $$
        \begin{bmatrix}
        a\\
        b\\
        c
        \end{bmatrix}
        $$

        - `d` is the shortest distance from the plane to the origin

          - $$
            d=P\cdot N
            $$

    - Without a plane equation:

      - 3 non-colinear points uniquely identifies a plane
      - Cross the 3 points to find the normal
      - Use the normal to find the plane equation

  - Polygon Attributes

    - Per vertex:
      - Position
      - Texture coordinates

    - Per vertex or per face (if flat shading)
      - Color
      - Normal

  - Reminder: What are Normals For?

    - Lighting

- Orthographic Projections

  - Projections on the z-axis

  - Parallel projection

    - All parallel projectors are perpendicular to each other, creating the projection on the projection plane from an object in eye space

    - $$
      PM=\begin{bmatrix}
      1&0&0&0\\
      0&1&0&0\\
      0&0&1&0\\
      0&0&0&1
      \end{bmatrix}
      $$

      - We want to keep the `z` information for later, so we keep the `z`, despite the fact that we're projecting onto the x-y plane

      - Normalized:

        - $$
          PM=\begin{bmatrix}
          \frac{2}{W}&0&0&0\\
          0&\frac{2}{H}&0&0\\
          0&0&\frac{1}{F-N}&-\frac{N}{F-N}\\
          0&0&0&1
          \end{bmatrix}
          $$

    - View Volume

      - $$
        -\frac{W}{2}\le X\le\frac{W}{2}\\
        -\frac{H}{2}\le Y\le\frac{H}{2}\\
        N\le Z\le F
        $$

      - We want to standardize the view volume into a canonical view volume to have standardized processes:

        - $$
          -1\le x\le1\\
          -1\le y\le1\\
          0\le z\le1
          $$

  - Perspective projection

    - All projectors originate from a single center of projection, creating the projection on the projection plane from an object in eye space

    - Point where all the objects are getting focused into (like the eye)

      - $$
        x\leftarrow\frac{x}{z}d\\
        y\leftarrow\frac{y}{z}d
        $$

    - $$
      PM=\begin{bmatrix}
      1&0&0&0\\
      0&1&0&0\\
      0&0&1&0\\
      0&0&\frac{1}{d}&0
      \end{bmatrix}
      $$

      - For a square viewport (aspect ratio is 1:1)

  - View Volumes

    - Front clipping plane prevents division by 0 errors

    - Back clipping plane prevents unnecessary computation

    - Half-angle of view limits sides of view

      - Defined in terms of the x-axis

    - Aspect ratio is the ratio of the width to the height of the final image

      - $$
        A_r=\frac{W}{H}
        $$




## Lecture 9: Projection Space and Screen Space

- Last Lecture Recap

  - Rendering Pipeline:
    - Model space
    - Object/world space
    - Eye/camera space
    - Projection space
    - Screen space

- Next Up

  - Geometric Calculations
  - Hidden Surface Removal
    - Backface culling
    - Object space and image space algorithms

  - Lighting
  - Flat and Smooth Shading

- Projection Space

  - Perspective Projection

    - We need to normalize the view volume from last lecture

    - $$
      -W\le x\le W\\
      -H\le y\le H\\
      N\le z\le F
      $$

    - Note that `W/d = tanθ` so when we normalize, where `θ` is the half-angle of view and is defined relative to the x-axis:

      - $$
        x'=\frac{x}{z}\frac{d}{W}=\frac{x}{z\tan\theta}\\
        y'=\frac{y}{z}\frac{d}{H}=\frac{y}{z}\frac{d}{W}\frac{W}{H}=\frac{yA_r}{z\tan\theta}
        $$

      - Since both normalized values have `z` in the denominator, we see that the farther away an object is, the smaller it will appear in projection space => perspective division

    - $$
      PM=\begin{bmatrix}
      1&0&0&0\\
      0&A_r&0&0\\
      0&0&1&0\\
      0&0&\tan\theta&0
      \end{bmatrix}
      $$

      - To retain `z` information:

        - $$
          0=A+\frac{B}{N}\\
          1=A+\frac{B}{F}\\
          A=\frac{F}{F-N}\\
          B=-\frac{FN}{F-N}
          $$

        - $$
          PM=\begin{bmatrix}
          1&0&0&0\\
          0&A_r&0&0\\
          0&0&A\tan\theta&B\tan\theta\\
          0&0&\tan\theta&0
          \end{bmatrix}
          $$

- Screen Space

  - Assume the viewport is defined by `Wxmin`, `Wymin`, `Wxmax`, and `Wymax`

    - $$
      W=W_{x_{max}}-W_{x_{min}}\\
      H=W_{y_{max}}-W_{y_{min}}
      $$

  - We need to translate (since the normalized window's bottom left is at `(-1, -1)`), scale, and translate back

    - $$
      T(W_{x_{min}},W_{y_{min}})S\left(\frac{W}{2},\frac{H}{2}\right)T(1,1)
      $$

    - The `2` in the denominator comes from the width/height of the window in projection space, so `W/2` and `H/2` are a ratio of the viewport size to the window size

- Overview

  - Model Space => Transformation Matrix => World Space => Eye Matrix => Eye Space => Projection Matrix => Projection Space => Normalized Projection Space => Window-To-Viewport Mapping => Screen Space
  - Theoretically, each matrix is a 4x4, so we should be able to combine all steps into a single matrix that moves from model space to screen space
    - Practically, the perspective division needed to normalize projection space stops us, so we can only go as far as projection space

- Geometric Calculations

  - Transforming Lines and Planes

    - Transforming Lines

      - Given by 2 endpoints

      - Given by line equation:

        - $$
          y=mx+b
          $$

          - Changing `m` is rotation:

            - $$
              m=\tan\theta
              $$

          - Changing `b` is translation in the `y` direction

    - Transforming Planes

      - Given by 3, non-colinear points

      - Given by a plane equation:

        - $$
          Ax+By+Cz+D=0
          $$

        - The normal is given by `(A, B, C)`

      - Given by a normal and a point:

        - $$
          n_x(x-p_x)+n_y(y-p_y)+n_z(z-p_z)=0
          $$

      - If `Mpoint` is a matrix to transform a point, then:

        - $$
          M_{normal}=(M_{point}^T)^{-1}
          $$

      - For orthogonal matrices:

        - $$
          M^T=M^{-1}\Rightarrow M_{normal}=M_{point}
          $$

  - Orthonormal Matrices

    - Consider upper-left 3x3 matrix

    - Each row is a unit vector

    - Each row is orthogonal to the others

      - Their dot product is 0

    - These vectors can be rotated to align the with xyz-axis

    - Determinant = 1

    - Inverse of orthogonal matrix:

      - $$
        M^{-1}=M^T
        $$

    - Preserves angles and lengths => rigid-body transformations

    - Examples of rigid-body transformations: translations, rotations

    - A sequence of orthonormal transformations results in a single orthonormal transformation

  - Point in Polygon Test

    - Convex polygons only: if point lies to the left of all edges, then it's inside the polygon

      - If it lies to the right of even one edge, it's outside

    - Semi-infinite ray:

      - From the point, shoot a semi-infinite ray in one direction

        - If the number of intersections is odd, then it is inside

      - $$
        (y_1>y_0\land y_2\le y_0)\lor(y_1\le y_0\land y_2>y_0)
        $$

        - `y0` is the middle vertex of 3 consecutive vertices

      - Intersection point `(x, y)`: `x > x0` for true intersection with semi-infinite ray to the right

    - Angle summation: if directed angle sum = 0, then outside, else inside

      - Connect point to each vertex and use directed angles between vertices

  - Normal Vector

    - 3 consecutive vertices (convex vertices): find cross product

    - Summation method:

      - $$
        \left(\sum(y_i-y_j)(z_i+z_j),\sum(z_i-z_j)(x_i+x_j),\sum(x_i-x_j)(y_i+y_j)\right)
        $$

      - Where `j = (i + 1) mod n` and `n` is the total number of vertices

      - Works for concave and even non-planar polygons

      - Not tested

  - Plane Equation

    - Surface normal and distance from origin:

      - $$
        n_xx+n_yy+n_zz=d
        $$

    - Three points on plane:

      - $$
        n_x(x-x_i)+n_y(y-y_i)+n_z(z-z_i)=0
        $$

  - On-Line Test

    - `P` is on `P1P2` means:

      - $$
        \frac{x-x_1}{y-y_1}=\frac{x_2-x_1}{y_2-y_1}
        $$

    - If:

      - $$
        T_{1,2}(P)=(x-x_1)(y_2-y_1)-(x_2-x_1)(y-y_1)
        $$

      - If positive, `P` is on the right, if negative, `P` is on the left

  - Edge-Edge Intersection

    - `P1` and `P2` are on opposite sides of line defined by `P3P4` and `P3` and `P4` are on opposite sides of line defined by `P1P2`

    - Equivalently, check for intersection:

      - $$
        (T_{1,2}(P_3)\times T_{1,2}(P_4)<0)\land(T_{3,4}(P_1)\times T_{3,4}(P_2)<0)
        $$

  - Collinearity Test

    - `t` is the distance from point `P` to line `P1P2`

    - `θ` is the angle between `P1P` and `P1P2`

    - $$
      t=|P_1P|\sin\theta=\frac{|P1P||P1P2|\sin\theta}{P_1P_2}=\frac{|P_1P\times P_1P_2|}{|P_1P_2|}
      $$

    - If `t < ε`, `P` is considered to be on `P1P2`




## Lecture 10: Hidden Surface Removal

- Next Up

  - Hidden Surface Removal
    - Backface culling
    - Painter's algorithm
    - Z-buffer algorithm
    - Scanline z-buffer algorithm

  - Lighting/Illumination Models
  - Flat and Smooth Shading
  - Shadow Algorithms
    - 2-pass z-buffer algorithm

  - Hidden Surface Removal
    - Ray casting, ray tracing

- Hidden Surface Removals

  - Object types:

    - Polymesh
    - Freeform surfaces
    - Volume
    - CSG
    - Implicit surfaces

  - Basic operations:

    - Establish priorities among polygons, objects, etc.
    - Collect overlapping elements and use priorities to resolve visibility

  - Backface Culling

    - `N` = outward normal vector of face

    - `P` = a point on face

    - `E` = eye vector (from a point on face to eye: `Eye - P`)

    - It is a front face if it's:

      - In world space:

        - $$
          N\cdot E>0
          $$

      - In eye/camera space:

        - $$
          N\cdot(-P)>0\text{ or }N\cdot P<0
          $$

      - In projection space (after perspective division):

        - $$
          N_z<0
          $$

  - Algorithm types:

    - Image space: operations 1 and 2, both at pixel resolution
    - List-priority: operation 1 at object resolution, operation 2 at pixel resolution
    - Object/world/eye space: operation 1 and 2, both at object resolution

  - Evaluation criteria:

    - Flexibility: what types of objects can it handle?
    - Special effects: transparency, antialiasing
    - Memory requirements
    - Speed

  - Object space algorithms:

    - Painter's

  - Image precision algorithms:

    - Z-buffer
    - Scanline z-buffer
    - Ray casting

  - Painter's Algorithm

    - Algorithm:
      - Sort polygons by z-depth
      - Scan-convert polygons in back-to-front order

    - Cannot handle certain cases:
      - Cyclic polygons
      - Intersecting polygons

  - Z-Buffer Algorithm

    - ```
      zb[Xres][Yres] = ∞
      cb[Xres][Yres] = background color
      
      For each polygon:
      	For each pixel covered by polygon:
      		Calculate z for polygon at (x, y)
      		If (z < zb[x][y]):
      			zb[x][y] = z
      			cb[x][y] = color of polygon
      ```

    - Properties:
      - Image precision algorithm
      - Easy to implement in software and hardware
      - Polygons scan-converted into framebuffer in random order
      - No pre-sorting of objects/polygons necessary
    - Disadvantages:
      - Memory requirements for storing color and depth for entire image
      - Aliasing issues
      - Complexity depends on polygon's projection area on the screen
      - Hard to handle transparency
    - Advantages:
      - Handles penetrating and cyclic objects
      - Extends to various kinds of faces other than polygons
      - Simplicity and ease of software implementation
      - Easily implementable in hardware
      - Be modified to reduce memory requirements
      - Can be extended to A-buffer to reduce aliasing
      - Theoretically, it can handle any number of polygons

  - Scanline Z-Buffer Algorithm

    - ```
      zb[Xres];
      cb[Xres];
      
      For each scanline (y = scanline, reset zb and cb):
      	For each polygon which intersects scanline:
      		Scan convert for specific scanline; determine span segment
      		For each pixel in span segment (x = pixel location):
      			Calculate z for polygon at (x, y)
      			If (z < zb[x]):
      				zb[x] = z
      				cb[x] = color of polygon
      ```

    - Advantages:

      - Same as z-buffer
      - Less memory requirement than z-buffer

    - Disadvantages:

      - Multiple passes through polygon database

  - Efficiency Considerations in Z-Buffer Algorithms

    - Speed considerations:

      - Bounding box testing

        - `Ymin/Ymax` test: associate `Ymin/Ymax` with each face
        - Calculate left and right ends: x-intercept with scanline

      - Incremental calculation of `Z` or bilinear interpolation

        - $$
          Ax+By+Cz+D=0\\
          z=-\frac{Ax+By+D}{C}\\
          z_{x+1}-z_x=-\frac{A}{C}\\
          z_{x+1}=z_x+\Delta z,\quad\text{where }\Delta z=-\frac{A}{C}
          $$

      - Space subdivision

      - Hierarchical subdivision



## Lecture 12: Illumination and Shading

- Lighting/Illumination

  - Types of Lighting:
    - Ambient
      - A crude approximation of real-life scenarios
      - Some background visibility of an object, but not really the shape of the object
    - Diffuse
      - Starts to give you some shape
      - Take into account the orientation of the object, normals, light vectors, etc.
    - Specular
      - Start noticing the highlights on shiny surfaces
  - Geometric Properties:
    - Object: position, orientation (normal)
    - Light: position, direction, point vs. spot vs. area
    - Eye: position, orientation
  - Material Properties:
    - Object: color, reflectivity, shininess, bumpiness, translucency
    - Light: color
    - Eye: filter, color blindness

- Ambient Lighting

  - Properties:

    - Background light

    - Unrealistic

    - Works as a good approximation of scattered light

    - Does *not* depend on position/orientation of light, object, or eye

    - Only depends on object and light's material properties

    - `ka` is the ambient reflection coefficient, values `[0..1]`

      - May be different for `R`, `G`, and `B`

    - `Ia` is the intensity of ambient light source, values `[0..1]`

      - Different for `R`, `G`, and `B`

    - Ambient light reflected off object:

      - $$
        k_a\times I_a
        $$

- Diffuse Lighting

  - Properties:

    - Point light source

    - Lambertian (or diffuse) reflection for dull, matte surfaces

    - Surfaces look equally bright from all directions

    - Reflect light *equally* in all directions

    - Lambert's Law: amount of light reflected from a differential unit area `dA` toward a viewer is proportional to the cosine of the angle between the incident light and the normal (`θ`)

    - `kd` is the diffuse reflection coefficient with values `[0..1]`

    - `Ip` is the intensity of point light source, values `[0..1]`

    - Diffuse light reflected off object:

      - $$
        k_d\times I_p\times\cos\theta=k_d\times I_p\times (N\cdot L)
        $$

  - Incident Angle `θ`:

    - `θ < 90º` => some light based on angle `θ`
    - `θ = 0º` => max light
    - `θ > 90º` => self-occlusion

  - Directional Light Source

    - A light at a sufficient distance from object (e.g., sun)
    - `L` remains the same for entire scene
    - `N` remains the same for entire polygon
    - Therefore, `N · L` is constant on the polygon, `L` is constant everywhere

  - Attenuated Light Source

    - Diffuse light reflected off object:

      - $$
        f_{att}\times k_d\times I_p\times\cos\theta=f_{att}\times k_d\times I_p\times(N\cdot L)
        $$

    - $$
      f_{att}=\frac{1}{d}\text{ or }\frac{1}{c_1+c_2\times d+c_3\times d^2},\quad f_{att}=\min(f_{att},1)
      $$

  - Colored Light and Objects

    - Object's diffuse color:

      - $$
        (O_{d\lambda}):O_{dR},O_{dG},O_{dB}
        $$

    - $$
      I_\lambda=[k_{a\lambda}\times I_{a\lambda}+f_{att}\times k_{d\lambda}\times I_{p\lambda}\times(N\cdot L)]\times O_{d\lambda}
      $$

  - Atmospheric Attenuation or Blending

    - Depth cueing or fog (fog color = `Idcλ`)

    - $$
      I_\lambda=s_o\times I_\lambda+(1-s_o)\times I_{dc\lambda}
      $$

    - $$
      s_o=s_b\text{ when }z>z_b
      $$

    - $$
      s_o=s_f\text{ when }z<z_f
      $$

    - $$
      s_o=s_f+\frac{s_b-s_f}{z_b-z_f}(z-z_f)
      $$

- Specular Lighting

  - Properties:

    - Shiny surfaces

    - Color of light, not object

    - Does depend on position of light, object, and eye

    - Light reflects unequally in different directions (e.g., perfect reflector: mirror)

    - For non-perfect reflectors

    - `ks` is the specular reflection coefficient, with values `[0..1]`

      - May be different for `R`, `G`, and `B`

    - `n` is the material's specular reflection exponent, values `[1..100s]`

      - Perfect reflector: `n = ∞`

    - Specular light reflected off object:

      - $$
        f_{att}\times k_s\times I_p\times\cos^n\phi=f_{att}\times k_s\times I_p\times(R\cdot V)^n
        $$

    - $$
      I_\lambda=k_{a\lambda}I_{a\lambda}O_{d\lambda}+f_{att}k_{d\lambda} I_{p\lambda}(N\cdot L)O_{d\lambda}+f_{att}k_{s\lambda}I_{p\lambda}(R\cdot V)^n
      $$

      - The ambient light source has no attenuation factor since it's a background light
      - The diffuse term is based on Lambert's Law (angle between the normal and the light vector)
      - The specular term is based on the angle between the reflection vector and the view vector

  - Specular Term: Smoothness Exponent Effect

    - Exponentiating a term that has values less than 1 draws it closer to 0

    - Higher exponent => smaller region where point light's reflection is considered aligned with the viewer => smaller shiny spot

    - Negative values of `cosΦ` is clamped to 0:

      - $$
        \max(0,(R\cdot V)^n)
        $$

    - Max specular reflection when `Φ = 0`

  - Calculating the `R` Vector

    - Reflection of point light source

    - Given normalized light vector `L` and the normal `N`

    - $$
      N'=(L\cdot N)N=L+u\\
      u=(L\cdot N)N-L\\
      \begin{equation*}\begin{split}
      R&=N'+u\\
      &=L+2u\\
      &=2(N\cdot L)N-L
      \end{split}\end{equation*}
      $$

      - `N'` is the projection of `L` onto `N`
      - `u` is the vector from the tip of `L` to the tip of `N'`

  - Halfway Vector: Alternate Formulation of `R · V`

    - Halfway vector (`H`) between `L` and `V`:

      - $$
        \text{normalize}(L+V)
        $$

    - Replace:

      - $$
        (R\cdot V)^n\text{ with }(H\cdot N)^n=\cos^n\psi,\quad\psi=\frac{\theta}{2}
        $$

    - This alternate is referred to as Blinn-Phong illumination

  - Final Light Equation

    - $$
      I=\text{emissive}+\text{ambient}+\text{diffuse}+\text{specular}\\
      \begin{equation*}
      \begin{split}
      \text{emissive}&=k_e\\
      \text{ambient}&=k_a\times ambientColor\\
      \text{diffuse}&=k_d\times lightColor\times\cos\theta\\
      &=k_d\times lightColor\times\max(0,N\cdot L)\\
      \text{specular}&=k_s\times lightColor\times\cos^n\phi\\
      &=k_s\times lightColor\times\max(0,R\cdot V)^n
      \end{split}
      \end{equation*}
      $$

- Lighting: Miscellaneous Improvements

  - Multiple Light Sources

    - Sum the light terms over all light sources
      - Usually only one ambient light source

  - Clamping

    - $$
      x=\max(0,x)\text{ and }\min(x,1)
      $$

    - With respect to the max value of color in the entire image:

      - $$
        x=\text{normalize}(x)
        $$

  - Fast Alternative to Phong Illumination

    - $$
      t=R\cdot V\text{ or }H\cdot N
      $$

    - Instead of `t^n`, do:

      - $$
        \frac{t}{n-nt+t}
        $$

  - Spot Lights

    - Smooth spot silhouette

    - If `α < β`, then illuminate the point

      - `β` is the spot angle

      - Replace angles with cosines

        - `β` is a constant, so `cosβ` is also a constant

        - $$
          \cos\alpha=L\cdot D
          $$

          - `L` is the light vector to the point you are trying to illuminate and `D` is the direction of the spot light

    - Center of the spot should be maximally lit, while there should be a soft falloff towards the periphery of the spot

      - Use `L · D` => maximum in the center, falls off towards the periphery
      - Clamp to 0 when the angle is larger than `β`

- Types of Lights: Summary

  - | Type        | Location | Direction        |
    | ----------- | -------- | ---------------- |
    | Ambient     | No       | No               |
    | Point       | Yes      | No               |
    | Directional | No       | Yes              |
    | Spot        | Yes      | Yes + Spot Angle |

- Flat/Constant/Faceted Shading

  - Use the `N` of the polygon
  - Find `I` at center/any vertex of the polygon
  - Apply that same color to all points inside the polygon
  - In essence, it means:
    - `N` is constant across the polygon
    - Light is at ∞ => `N · L` is constant across the polygon
    - Viewer is at ∞ => `N · V` is constant across the polygon
  - Which space to compute `N` and illuminate?
    - Either world space or eye space, not in projection space
  - Problem with Flat Shading
    - Mach bands are an optical illusion, exaggerating the contrast between edges of slightly differing shades of gray

- Gouraud Smooth Shading

  - Also called intensity interpolation or color interpolation shading

    - $$
      C=(1-\alpha)C_1+\alpha C_2
      $$

    - Bilinear interpolation: linear interpolation in 2-dimensions

  - World space: find `I` at each vertex and illuminate the vertex

  - Screen space: interpolate across the polygon face

  - Store normals at vertices

    - Average normals of polygons sharing a vertex
    - During tessellation, e.g., sphere
    - What about hard edges?

  - Issues with Gouraud Shading

    - Rotating polygons
    - Specular reflection with large polygons
      - At polygon's center
      - At polygon's vertex
    - Mach banding is not completely eliminated



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