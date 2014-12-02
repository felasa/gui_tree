#WIDGETS

require(gWidgets)
require(gWidgetstcltk)
options("guiToolkit"="tcltk")
obj <- gbutton("Hello world", container = gwindow())
obj <- glabel("Hello world", container = gwindow())
obj <- gspinbutton(from=0, to = 7734, by =100, value=0,
                   container=gwindow())

win <- gwindow("Hello World, ad nauseum", visible=TRUE)
group <- ggroup(horizontal = FALSE, container=win)
obj <- gbutton("Hello...",container=group,
               handler = function(h,...) gmessage("world"))
obj <- glabel("Hello...", container =group,
              handler = function(h,...) gmessage("world"))

board <- matrix ( rep ( 0 , 9 ) , nrow=3)
layout_board <- function ( ) {
  plot.new( )
  plot.window( xlim=c( 1 , 4 ) , ylim=c( 1 , 4 ) )
  abline( v = 2 : 3 ) ; abline ( h =2 : 3 )
  mtext( "Tic Tac Toe. Click a square:" )
}
do_play <- function( ) {
  iloc <- locator( n=1 , type="n" )
  click_handler( iloc )
}
click_handler <- function( iloc ) {
  if ( is.null( iloc ) )
    stop ( "Game terminated early" )
  move <- floor( unlist( iloc ) )
  draw_move( move , "x" )
  board [3 * ( move[2]-1) + move [ 1 ] ] <<- 1
  if ( !is_finished( ) )
    do_computer_move( )
  if ( !is_finished( ) ) {
    do_play( )
  }
}

draw_move <- function( z , type="x" ) {
  i <- max( 1 , min( 3 , z [ 1 ] ) ) ; j <- max( 1 , min ( 3 , z[ 2 ] ) )
  if ( type == "x" ) {
    lines( i + c( . 1 , . 9 ) , j + c( . 1 , . 9 ) )
    lines( i + c( . 1 , . 9 ) , j + c( . 9 , . 1 ) )
  } else {
    theta <- seq( 0 , 2 *pi , length =100)
    lines ( i + 1/2 + . 4 *cos ( theta ) , j + 1/2 + . 4 *sin ( theta ) )
  }
}
