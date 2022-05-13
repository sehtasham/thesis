# Create 3d graphs using the outer product of arrays

create_3d_graph <- function(X, Y, FUNC, zlabel){
    Z <- outer(X, Y, FUNC)
    persp(X, Y, Z,
    xlab = "lambda",
    ylab = "p_o",
    zlab = zlabel,
    theta = 0, phi = 0,
    col = "springgreen", shade = 0.5)
}
