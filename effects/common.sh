# Helper functions for text rendering

# Render using ImageMagick and pango to a frame of 40x7 pixels using
# given threshold (darkness) in the font. Defaults to 50% which is
# quite OK.  Outputs raw binary bitmap.
render_text() {
    convert -gravity center +antialias +dither \
	    -font Ubuntu -pointsize 6 -define pango:markup=false \
	    "pango:$1" \
	    -gravity north -crop '40x7+1+1!' -threshold ${2-50}% \
	    -background black -negate -gravity west -flatten png:-
}

send_to_screen() {
    # Set serial port parameters if outputting to a char device
    nc -u "$1" "$2"
}
