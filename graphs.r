# Create 3d graphs using the outer product of arrays

create_3d_graph <- function(X, Y, FUNC, zlabel){
    Z <- outer(X, Y, FUNC)
    persp(X, Y, Z,
    xlab = "Traffic Load (%)",
    ylab = "Offloading Probability (%)",
    zlab = "",
    theta = 30, phi = 0,
    ticktype = "detailed",
    col = "springgreen", shade = 0.5)
    mtext(zlabel, side = 2)
}
