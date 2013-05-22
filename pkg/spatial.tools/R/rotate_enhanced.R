


rotate_enhanced <- function(x,rotation=-180,...)
{
	x_extent <- extent(x)
	extent(x) <- extent(0,360,x_extent@ymin,x_extent@ymax)
	x_rotated <- rotate(x)
	
	
}