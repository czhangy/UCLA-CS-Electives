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

        - `P2` is the head (arrow-end), `P1` is the tail

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
        |\bold{u}|\cos\theta-\frac{\bold{u}\cdot\bold{v}}{|\bold{v}|}
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



## Lecture 7:

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



## Lecture 8:

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