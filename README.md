# LG Chess

A class project in which I apply Linguistic Geometry to chess. I decided to use Racket because I don't know any other LISP style languages and I thought it was about time I learned one.

## Definitions
Chess = <X, P, Rp, {ON}, v, Si, St, TR>

X = {xi} is a finite state of points
    xi is each point on the chess board. I chose to represent x0 as bottom left and x64 as top right

P = P1 ∪ P2 is a finite set of pieces, P1 ∩ P2 ≠ Ø
    P1 is the first player's pieces
    P2 is the second player's pieces

    p0 = Pawn (0, 1)
    p1 = Pawn (1, 1)
    p2 = Pawn (2, 1)
    p3 = Pawn (3, 1)
    p4 = Pawn (4, 1)
    p5 = Pawn (5, 1)
    p6 = Pawn (6, 1)
    p7 = Pawn (7, 1)
    p8 = Rook (0, 0)
    p9 = Knight (1, 0)
    p10 = Bishop (2, 0)
    p11 = Queen (3, 0)
    p12 = King (4, 0)
    p13 = Bishop (5, 0)
    p14 = Knight (6, 0)
    p15 = Rook (7, 0)

Rp(x,y) = a family of binary relations of reachability in X
    (x ∈ X, y ∈ X, p ∈ P); y is reachable from x for p
    These isn't very valuable except for bishops which can only access their own color

ON(p) = x is a partial function of placement of pieces P into X


... TO BE CONTINUED
