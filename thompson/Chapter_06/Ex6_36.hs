module Chapter_06.Ex6_36 where

import Chapter_06.Ex6_35
import Chapter_06.Ex6_32 (printIm)
import Chapter_06.Ex6_15 (invertColor)

-----------------------------------------------------------
-- Exercise 6.36

-- | Superimposes two images
superimpose :: Image -> Image -> Image
superimpose img1 ([], _) = img1
superimpose ([], _) img2 = img2
superimpose (pic1, (x1, y1)) (pic2, (x2, y2)) = 
  (newPic, (x, y))
  where
    -- Dimensions of the input pictures
    h1 = length pic1
    w1 = if null pic1 then 0 else length (head pic1)
    h2 = length pic2
    w2 = if null pic2 then 0 else length (head pic2)

    -- New reference point and dimensions
    x = min x1 x2
    y = min y1 y2
    w = max (x1 + w1) (x2 + w2) - x
    h = max (y1 + h1) (y2 + h2) - y
    
    -- Padding for pic1
    lft1 = x1 - x
    btm1 = y1 - y
    rgt1 = w - (lft1 + w1)
    top1 = h - (btm1 + h1)
    
    -- Padding for pic2
    lft2 = x2 - x
    btm2 = y2 - y
    rgt2 = w - (lft2 + w2)
    top2 = h - (btm2 + h2)
    
    -- Pad both pictures to the same size 
    -- We ignore the new positions returned by pad
    -- since we already know the new reference point (x,y)
    pic1' = fst $ pad lft1 rgt1 btm1 top1 (pic1, (x1, y1))
    pic2' = fst $ pad lft2 rgt2 btm2 top2 (pic2, (x2, y2))
    
    -- Superimpose the two padded pictures by merging the
    -- characters, with '#' taking precedence over '.'
    newPic = zipWith (zipWith overlay) pic1' pic2'
    overlay '#' _ = '#'
    overlay  _  c =  c

-- | Inverts the colours of an image
invertIm :: Image -> Image
invertIm (pic, pos) = (invertColor pic, pos)

-- | Example image with a delta shape
delta :: Image
delta = (deltaPic, (4,7))
  where 
  deltaPic = 
    ["..............",
     ".....##.......",
     "....#.........",
     ".....##.......",
     ".....###......",
     "...##....#....",
     "..##......#...",
     "..##.....##...",
     "...######.....",
     ".............."]
 
-- | Example image with a lambda shape
lambda :: Image
lambda = (lambdaPic, (-3,0))
  where 
  lambdaPic =
    ["..............",
     "...##.........",
     ".....#........",
     "......#.......",
     "......##......",
     ".....#.##.....",
     "....#...##....",
     "...##....##...",
     "..###...####..",
     ".............."]


-----------------------------------------------------------

{-

In order to define a superimposition function for images,
Figure 6.11 is very instructive. Let's call the two images
to be superimposed Img1 = (pic1, (x1,y1)), with height h1
and width w1, and Img2 = (pic2, (x2,y2)), with height h2 
and width w2. The resulting image will be Img = (pic,
(x,y)), with height h and width w.

In the figure, the two images have different reference 
points, and the resulting image's bottom-left corner is 
determined by the minimum of the two reference points, so:

  x = min x1 x2
  y = min y1 y2

The resulting picture's upper-right corner is determined
by the maximum of the two images' upper-right corners, 
(x1 + w1, y1 + h1) and (x2 + w2, y2 + h2), so:

  (x + w, y + h) = (max (x1 + w1) (x2 + w2),
                    max (y1 + h1) (y2 + h2))

From these two equations we can derive the width and
height of the resulting picture:

  w = max (x1 + w1) (x2 + w2) - x
  h = max (y1 + h1) (y2 + h2) - y

The resulting picture is then obtained by padding both pic1
and pic2 so that they have the same dimensions, and then
superimposing them. The padding of the input images is 
determined by the differences between their reference 
points and the reference point of the resulting picture, 
and the differences between their top-right corners and the 
top-right corner of the resulting picture. Specifically:

  Padding for pic1:
    left1   = x1 - x
    bottom1 = y1 - y
    right1  = w - (left1 + w1)
    top1    = h - (bottom1 + h1)

  Padding for pic2: 
    left2   = x2 - x
    bottom2 = y2 - y
    right2  = w - (left2 + w2)
    top2    = h - (bottom2 + h2)
  

These padded pictures are then superimposed character by
character, with the rule that a non-blank character in
either picture takes precedence over a blank character 
in the other picture. We already defined functions for
this in exercises 6.4-6.6, but here we choose to implement
them using zipWith for conciseness.

Testing in GHCi

ghci> :set -i..
ghci> :l Ex6_36
ghci> printIm $  invertIm $ superImpose delta lambda
#####################
############..#######
###########.#########
############..#######
############...######
##########..####.####
#########..######.###
#########..#####..###
###..#####......#####
#####.###############
######.##############
######..#############
#####.#..############
####.###..###########
###..####..##########
##...###....#########
#####################

-}