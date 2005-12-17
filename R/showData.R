R.to.Tcl <-
function (character.vector)
##  converts a character vector into a brace-delimited Tcl list
{
    if (length(character.vector) == 0) list()
    else paste("{", paste(character.vector, collapse = "} {"),
               "}",
               sep = "")
}

Tcl.to.R <-
function (tcl.list)
##  converts a fully brace-delimited Tcl list into a character
##  vector in R
{
    tcl.list <- substring(tcl.list, 2, nchar(tcl.list) - 1)
    strsplit(tcl.list, split = "} {", fixed = TRUE)[[1]]
}

"showData" <-
    function (dataframe,
              colname.bgcolor = "grey50",
              rowname.bgcolor = "grey50",
              body.bgcolor = "white",
              colname.textcolor = "white",
              rowname.textcolor = "white",
              body.textcolor = "black",
              font = "Courier 12",
              maxheight = 30,
              maxwidth = 80,
              title = NULL,
              rowname.bar = "left",
              colname.bar = "top",
              rownumbers = FALSE,
              placement = "-20-40")
{
    object.name <- deparse(substitute(dataframe))
    if (!is.data.frame(dataframe))
        stop(paste(object.name, "is not a data frame"))
    if (is.numeric(rownumbers) &&
        length(rownumbers) != nrow(dataframe))
        stop("rownumbers argument must be TRUE, FALSE or have length nrow(dataframe)")
    require(tcltk) || stop("Tcl/Tk support is absent")
    oldwidth <- unlist(options("width"))
    options(width = 10000)
    conn <- file()
    sink(conn)
    print(dataframe)
    sink()
    zz <- scan(conn, sep = "\n", what = character(0), quiet = TRUE)
    close(conn)
    if (length(zz) > 1 + nrow(dataframe)) stop(
       "data frame too wide")
    options(width = oldwidth)
    base <- tktoplevel()
    tkwm.geometry(base, placement)
    tkwm.title(base, {
        if (is.null(title))
            object.name
        else title
    })
    nrows <- length(zz) - 1
    if (is.numeric(rownumbers))
        rowname.text <- paste(rownumbers, row.names(dataframe))
    else if (rownumbers)
        rowname.text <- paste(1:nrows, row.names(dataframe))
    else rowname.text <- row.names(dataframe)
    namewidth = max(nchar(rowname.text))
    yy <- substring(zz, 2 + max(nchar(row.names(dataframe))))
    datawidth <- max(nchar(yy))
    winwidth <- min(1 + datawidth, maxwidth)
    hdr <- tktext(base,
                  bg = colname.bgcolor,
                  fg = colname.textcolor,
                  font = font,
                  height = 1,
                  width = winwidth,
                  takefocus = TRUE,
                  exportselection = TRUE)
    ftr <- tktext(base,
                  bg = colname.bgcolor,
                  fg = colname.textcolor,
                  font = font,
                  height = 1,
                  width = winwidth,
                  takefocus = TRUE)
    textheight <- min(maxheight, nrows)
    txt <- tktext(base,
                  bg = body.bgcolor,
                  fg = body.textcolor,
                  font = font,
                  height = textheight,
                  width = winwidth,
                  setgrid = 1,
                  takefocus = 1)
    lnames <- tktext(base,
                     bg = rowname.bgcolor,
                     fg = rowname.textcolor,
                     font = font,
                     height = textheight,
                     width = namewidth,
                     takefocus = TRUE)
    rnames <- tktext(base,
                     bg = rowname.bgcolor,
                     fg = rowname.textcolor,
                     font = font,
                     height = textheight,
                     width = namewidth,
                     takefocus = TRUE)
    xscroll <- tkscrollbar(base,
                           orient = "horizontal",
                           repeatinterval = 1,
                           command = function(...) {
                               tkxview(txt, ...)
                               tkxview(hdr, ...)
                               tkxview(ftr, ...)
                           })
    string.to.vector <- function(string.of.indices) {
        string.of.indices <- tclvalue(string.of.indices)
        as.numeric(strsplit(string.of.indices, split = " ")[[1]])
    }
    tkconfigure(txt, xscrollcommand = function(...) {
        tkset(xscroll, ...)
        xy <- string.to.vector(tkget(xscroll))
        tkxview.moveto(hdr, xy[1])
        tkxview.moveto(ftr, xy[1])
    })
    tkconfigure(hdr, xscrollcommand = function(...) {
        tkset(xscroll, ...)
        xy <- string.to.vector(tkget(xscroll))
        tkxview.moveto(txt, xy[1])
        tkxview.moveto(ftr, xy[1])
    })
    tkconfigure(ftr, xscrollcommand = function(...) {
        tkset(xscroll, ...)
        xy <- string.to.vector(tkget(xscroll))
        tkxview.moveto(hdr, xy[1])
        tkxview.moveto(txt, xy[1])
    })
    yscroll <- tkscrollbar(base,
                           orient = "vertical",
                           repeatinterval = 1,
                           command = function(...) {
                               tkyview(txt, ...)
                               tkyview(lnames, ...)
                               tkyview(rnames, ...)
                           })
    tkconfigure(txt, yscrollcommand = function(...) {
        tkset(yscroll, ...)
        xy <- string.to.vector(tkget(yscroll))
        tkyview.moveto(lnames, xy[1])
        tkyview.moveto(rnames, xy[1])
    })
    tkconfigure(lnames, yscrollcommand = function(...) {
        tkset(yscroll, ...)
        xy <- string.to.vector(tkget(yscroll))
        tkyview.moveto(txt, xy[1])
        tkyview.moveto(rnames, xy[1])
    })
    tkconfigure(rnames, yscrollcommand = function(...) {
        tkset(yscroll, ...)
        xy <- string.to.vector(tkget(yscroll))
        tkyview.moveto(txt, xy[1])
        tkyview.moveto(lnames, xy[1])
    })
    tkbind(txt, "<B2-Motion>", function(x, y) {
        tkscan.dragto(txt, x, y)
    })
## The next block just enables copying from the text boxes
{
    copyText.hdr <- function(){
        tkcmd("event", "generate",
              .Tk.ID(hdr),
              "<<Copy>>")}
    tkbind(hdr, "<Button-1>", function() tkfocus(hdr))
    editPopupMenu.hdr <- tkmenu(hdr, tearoff = FALSE)
    tkadd(editPopupMenu.hdr, "command", label = "Copy <Ctrl-C>",
              command = copyText.hdr)
    RightClick.hdr <- function(x,y) # x and y are the mouse coordinates
    {
        rootx <- as.integer(tkwinfo("rootx", hdr))
        rooty <- as.integer(tkwinfo("rooty", hdr))
        xTxt <- as.integer(x) + rootx
        yTxt <- as.integer(y) + rooty
        tkcmd("tk_popup", editPopupMenu.hdr, xTxt, yTxt)
    }
    tkbind(hdr, "<Button-3>", RightClick.hdr)
    tkbind(hdr, "<Control-KeyPress-c>", copyText.hdr)
    ##
    copyText.ftr <- function(){
        tkcmd("event", "generate",
              .Tk.ID(ftr),
              "<<Copy>>")}
    tkbind(ftr, "<Button-1>", function() tkfocus(ftr))
    editPopupMenu.ftr <- tkmenu(ftr, tearoff = FALSE)
    tkadd(editPopupMenu.ftr, "command", label = "Copy <Ctrl-C>",
              command = copyText.ftr)
    RightClick.ftr <- function(x,y) # x and y are the mouse coordinates
    {
        rootx <- as.integer(tkwinfo("rootx", ftr))
        rooty <- as.integer(tkwinfo("rooty", ftr))
        xTxt <- as.integer(x) + rootx
        yTxt <- as.integer(y) + rooty
        tkcmd("tk_popup", editPopupMenu.ftr, xTxt, yTxt)
    }
    tkbind(ftr, "<Button-3>", RightClick.ftr)
    tkbind(ftr, "<Control-KeyPress-c>", copyText.ftr)
    ##
    copyText.txt <- function(){
        tkcmd("event", "generate",
              .Tk.ID(txt),
              "<<Copy>>")}
    tkbind(txt, "<Button-1>", function() tkfocus(txt))
    editPopupMenu.txt <- tkmenu(txt, tearoff = FALSE)
    tkadd(editPopupMenu.txt, "command", label = "Copy <Ctrl-C>",
              command = copyText.txt)
    RightClick.txt <- function(x,y) # x and y are the mouse coordinates
    {
        rootx <- as.integer(tkwinfo("rootx", txt))
        rooty <- as.integer(tkwinfo("rooty", txt))
        xTxt <- as.integer(x) + rootx
        yTxt <- as.integer(y) + rooty
        tkcmd("tk_popup", editPopupMenu.txt, xTxt, yTxt)
    }
    tkbind(txt, "<Button-3>", RightClick.txt)
    tkbind(txt, "<Control-KeyPress-c>", copyText.txt)
    ##
    copyText.lnames <- function(){
        tkcmd("event", "generate",
              .Tk.ID(lnames),
              "<<Copy>>")}
    tkbind(lnames, "<Button-1>", function() tkfocus(lnames))
    editPopupMenu.lnames <- tkmenu(lnames, tearoff = FALSE)
    tkadd(editPopupMenu.lnames, "command", label = "Copy <Ctrl-C>",
              command = copyText.lnames)
    RightClick.lnames <- function(x,y) # x and y are the mouse coordinates
    {
        rootx <- as.integer(tkwinfo("rootx", lnames))
        rooty <- as.integer(tkwinfo("rooty", lnames))
        xTxt <- as.integer(x) + rootx
        yTxt <- as.integer(y) + rooty
        tkcmd("tk_popup", editPopupMenu.lnames, xTxt, yTxt)
    }
    tkbind(lnames, "<Button-3>", RightClick.lnames)
    tkbind(lnames, "<Control-KeyPress-c>", copyText.lnames)
    ##
        copyText.rnames <- function(){
        tkcmd("event", "generate",
              .Tk.ID(rnames),
              "<<Copy>>")}
    tkbind(rnames, "<Button-1>", function() tkfocus(rnames))
    editPopupMenu.rnames <- tkmenu(rnames, tearoff = FALSE)
    tkadd(editPopupMenu.rnames, "command", label = "Copy <Ctrl-C>",
              command = copyText.rnames)
    RightClick.rnames <- function(x,y) # x and y are the mouse coordinates
    {
        rootx <- as.integer(tkwinfo("rootx", rnames))
        rooty <- as.integer(tkwinfo("rooty", rnames))
        xTxt <- as.integer(x) + rootx
        yTxt <- as.integer(y) + rooty
        tkcmd("tk_popup", editPopupMenu.rnames, xTxt, yTxt)
    }
    tkbind(rnames, "<Button-3>", RightClick.rnames)
    tkbind(rnames, "<Control-KeyPress-c>", copyText.rnames)
}

    tktag.configure(hdr, "notwrapped", wrap = "none")
    tktag.configure(ftr, "notwrapped", wrap = "none")
    tktag.configure(txt, "notwrapped", wrap = "none")
    tktag.configure(lnames, "notwrapped", wrap = "none")
    tktag.configure(rnames, "notwrapped", wrap = "none")
    tkinsert(txt, "end", paste(paste(yy[-1], collapse = "\n"),
                               sep = ""), "notwrapped")
    tkgrid(txt, row = 1, column = 1, sticky = "nsew")
    if ("top" %in% colname.bar) {
        tkinsert(hdr, "end", paste(yy[1], sep = ""), "notwrapped")
        tkgrid(hdr, row = 0, column = 1, sticky = "ew")
    }
    if ("bottom" %in% colname.bar) {
        tkinsert(ftr, "end", paste(yy[1], sep = ""), "notwrapped")
        tkgrid(ftr, row = 2, column = 1, sticky = "ew")
    }
    if ("left" %in% rowname.bar) {
        tkinsert(lnames, "end",
                 paste(rowname.text, collapse = "\n"),
                 "notwrapped")
        tkgrid(lnames, row = 1, column = 0, sticky = "ns")
    }
    if ("right" %in% rowname.bar) {
        tkinsert(rnames, "end",
                 paste(rowname.text, collapse = "\n"),
                 "notwrapped")
        tkgrid(rnames, row = 1, column = 2, sticky = "ns")
    }
    tkconfigure(hdr, state = "disabled")
    tkconfigure(ftr, state = "disabled")
    tkconfigure(txt, state = "disabled")
    tkconfigure(lnames, state = "disabled")
    tkconfigure(rnames, state = "disabled")
    if (maxheight < nrows) {
        tkgrid(yscroll, row = 1, column = 3, sticky = "ns")
    }
    if (maxwidth < datawidth) {
        tkgrid(xscroll, row = 3, column = 1, sticky = "ew")
    }
    tkgrid.rowconfigure(base, 1, weight = 1)
    tkgrid.columnconfigure(base, 1, weight = 1)
    tkwm.maxsize(base, 1 + datawidth, nrows)
    tkwm.minsize(base, 1 + nchar(names(dataframe)[1]), 1)
    invisible(NULL)
}
