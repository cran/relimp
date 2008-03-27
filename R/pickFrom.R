"pickFrom" <-  function (vec, nsets = 1, return.indices = FALSE,
                         setlabels = NULL,
                         edit.setlabels = TRUE,
                         subset = TRUE,
                         warningText = "one or more selections empty",
                         title = "Subset picker",
                         items.label = "Pick from:",
                         labels.prompt = "Your label for this set:",
                         list.height = 20,
                         items.scrollbar = TRUE,
                         preserve.order = TRUE,
                         listFont = "Courier 12",
                         labelFont = "Helvetica 11",
                         windowPos = "+150+30")
{
    ppp <- NULL ## only to avoid a NOTE at package check time
    if (!is.vector(vec))
        stop("argument `vec' muct be a vector")
    vec.is.numeric <- if (is.numeric(vec)) TRUE else FALSE
    vec.as.char <- as.character(vec)

    if (is.character(subset)) subset <- match(subset(names(vec)))
    if (is.logical(subset)) subset <- seq(along = vec)[subset]

    require(tcltk) || stop("tcltk support is absent")
    string.to.vector <- function(string.of.indices) {
        as.numeric(strsplit(string.of.indices, split = " ")[[1]])
    }
    base <- tktoplevel(takefocus = 1)
    tkwm.title(base, title)
    tkwm.geometry(base, windowPos)
    tkwm.resizable(base, 0, 0)

    right.frm <- tkframe(base)
    left.frm <- tkframe(base)

    vec.to.pickfrom <- vec.as.char[subset]

    items.list <- as.character(tclVar(paste("{",
                                 paste(vec.to.pickfrom, collapse = "} {"), "}",
                                 sep = "")))
    items.frm <- tkframe(left.frm)

    items.label <- tklabel(items.frm,
                           text = items.label,
                           anchor = "w",
                           justify = "left")
    tkgrid(items.label, row = 0, columnspan = 2, sticky = "w")
    items.height <- min(list.height, length(vec.to.pickfrom))
    items.width <- max(8, max(nchar(vec.to.pickfrom)))
    items <- tklistbox(items.frm,
                       listvar = items.list,
                       bg = "grey50",
                       selectmode = "extended",
                       fg = "white",
                       font = listFont,
                       width = items.width,
                       height = items.height)
    tkgrid(items, row = 1, column = 0)
    preserve.order <- tclVar(as.numeric(preserve.order))
    buttons.frm <- tkframe(left.frm)
    buttonA <- tkradiobutton(buttons.frm,
                   text = "Sort sets in\nthe above order\nupon \"Add\"",
                             justify = "left",
                             variable = preserve.order,
                             value = "1",
                             command = function(){NULL}
                             )

    buttonB <- tkradiobutton(buttons.frm,
                             text = "Place\nnewly added\nitems last",
                             justify = "left",
                             variable = preserve.order,
                             value = "0",
                             command = function(){NULL}
                             )

    if (items.scrollbar && (length(vec) > items.height)) {
        items.scrollbar <- tkscrollbar(items.frm,
                           orient = "vertical",
                           repeatinterval = 1,
                           command = function(...) {
                              tkyview(items, ...)
                           })
        tkconfigure(items, yscrollcommand = function(...) {
            tkset(items.scrollbar, ...)
            xy <- string.to.vector(tclvalue(tkget(items.scrollbar)))
            tkyview.moveto(items, xy[1])
        })
    tkgrid(items.scrollbar, row = 1, column = 1, sticky = "ns")
    }
    tkpack(buttonA, buttonB, pady = 1, padx = 5, side = "top",
           anchor = "nw")
    tkpack(items.frm, buttons.frm,
           pady = 1, padx = 5, side = "top")

    tkpack(left.frm, side = "top", expand = "true", anchor = "n")

    sets.frm <- tkframe(right.frm)
    setframe <- list()
    label <- list()
    setlabeltext <- list()
    setlabels <- if (!is.null(setlabels))
        as.list(setlabels)
    else as.list(rep("", nsets))
    labelentry <- list()
    TCLlabel <- list()
    listbox <- list()
    add.but <- list()
    labelbox <- list()
    listvarname <- list()
    remove.but <- list()
    tkset <- list()
    set <- list()
    Rtkset <- list()
    subset.height <- min(list.height - 5, length(vec.to.pickfrom))
    for (i in 1:nsets) {
        tkset[[i]] <- tclVar("")
        TCLlabel[[i]] <- tclVar(setlabels[[i]])
        setframe[[i]] <- tkframe(sets.frm,
                                 width = 250,
                                 relief = "groove",
                                 borderwidth = 2
                                 )
        label[[i]] <- tklabel(setframe[[i]], text = setlabels[[i]]
                              )
        listvarname[[i]] <- as.character(tkset[[i]])
        listbox[[i]] <- tklistbox(setframe[[i]],
                                  listvar = listvarname[[i]],
                                  bg = "white",
                                  height = subset.height,
                                  font = listFont,
                                  width = items.width,
                                  selectmode = "extended")
        labelbox[[i]] <- tkframe(setframe[[i]], width = 250)
        setlabeltext[[i]] <- tklabel(labelbox[[i]],
                                     text = labels.prompt)
    }
    add.cmd <- deparse(function() {
        set[[ppp]] <- match(Tcl.to.R(tclvalue(tkset[[ppp]])),
                            vec.to.pickfrom)
        set[[ppp]] <- union(set[[ppp]],
                           1 + string.to.vector(tclvalue(
                                                  tkcurselection(items))))
        if (as.logical(tclObj(preserve.order))) set[[ppp]] <- sort(set[[ppp]])
        tclvalue(tkset[[ppp]]) <- R.to.Tcl(vec.to.pickfrom[set[[ppp]]])
        tkconfigure(add.but[[ppp]], state = "disabled")
    })
    remove.cmd <- deparse(function() {
        Rtkset[[ppp]] <- Tcl.to.R(tclvalue(tkset[[ppp]]))
        out <- 1 + string.to.vector(tclvalue(tkcurselection(listbox[[ppp]])))
        if (length(Rtkset[[ppp]]) == length(out)) tclvalue(tkset[[ppp]]) <- ""
        else tclvalue(tkset[[ppp]]) <- R.to.Tcl(Rtkset[[ppp]][-out])
        tkconfigure(remove.but[[ppp]], state = "disabled")
        tkselection.clear(listbox[[ppp]], "0", "end")
    })
    for (i in 1:nsets) {
        add.but[[i]] <- tkbutton(setframe[[i]], text = "Add",
                                 fg = "darkgreen",
                                 disabledforeground = "darkgrey",
                                 width = 10,
                                 state = "disabled",
                                 command = eval(parse(text = gsub("ppp",
                                                   as.character(i), add.cmd))))
        remove.but[[i]] <- tkbutton(setframe[[i]], text = "Remove",
                                    fg = "darkred",
                                    disabledforeground = "darkgrey",
                                    width = 10,
                                    state = "disabled",
                                    command = eval(parse(text = gsub("ppp",
                                                as.character(i), remove.cmd))))
        labelentry[[i]] <- tkentry(labelbox[[i]],
                                   textvariable = as.character(TCLlabel[[i]]),
                                   font = labelFont,
                                   bg = "white")
        if (edit.setlabels) {
            tkpack(setlabeltext[[i]], labelentry[[i]], side = "top",
               anchor = "w")
        }
        tkpack(label[[i]], add.but[[i]], remove.but[[i]], listbox[[i]],
               labelbox[[i]], side = "top", padx = 5, pady = 5)
        tkpack(setframe[[i]], side = "left", padx = 3, pady = 10)
    }
    fun1 <- deparse(function() {
        if (tclvalue(tkcurselection(listbox[[ppp]])) != "") {
            for (j in 1:nsets) {
                tkconfigure(add.but[[j]], state = "disabled")
            }
            tkconfigure(remove.but[[ppp]], state = "normal")
        }
        for (j in (1:nsets)[-ppp]) {
            tkconfigure(remove.but[[j]], state = "disabled")
        }
        tkfocus(listbox[[ppp]])
    })
    for (i in 1:nsets) {
        tkbind(listbox[[i]], "<<ListboxSelect>>",
                eval(parse(text = gsub("ppp", as.character(i), fun1))))
    }
    tkbind(items, "<<ListboxSelect>>", function() {
        items.selected <- vec.to.pickfrom[1 + string.to.vector(tclvalue(
                                                 tkcurselection(items)))]
        for (i in 1:nsets) {
            set[[i]] <- Tcl.to.R(tclvalue(tkset[[i]]))
            if (setequal(items.selected, intersect(items.selected,
                                                   set[[i]]))) {
                tkconfigure(add.but[[i]], state = "disabled")
            }
            else tkconfigure(add.but[[i]], state = "normal")
            tkconfigure(remove.but[[i]], state = "disabled")
        }
    })
    tkbind(items, "<Button-1>", function() tkfocus(items))
    buttons.frame <- tkframe(right.frm)
    OK <- tclVar(0)
    ok.but <- tkbutton(buttons.frame, text = "OK", width = 10,
                       command = function() {
                           tkdestroy(base)
                           tclvalue(OK) <- 1
                       })
    tkconfigure(ok.but, state = "normal")
    cancel.but <- tkbutton(buttons.frame, text = "Cancel", width = 10,
                           command = function() {
                               tkdestroy(base)
                           })
    tkpack(ok.but, cancel.but, side = "left", padx = 20, pady = 20)
    tkpack(sets.frm, buttons.frame, side = "top")
    tkpack(left.frm, side = "left", anchor = "nw", padx = 1)
    tkpack(right.frm, anchor = "ne")
    tkwait.window(base)
    .Tcl("update idletasks")
    if (tclvalue(OK) == "1") {
        sets <- lapply(tkset, function(set) {
            match(Tcl.to.R(tclvalue(set)), vec.to.pickfrom)
        })
        if (any(sapply(sets, length) == 0)) {
            warning(warningText)
        }
        labels <- lapply(TCLlabel, tclvalue)
        names(sets) <- labels
        result <- sets
    } else return(NULL)
    return(
           if (return.indices)
           lapply(result, function(set) subset[set])
           else lapply(result, function(set) (vec[subset])[set])
           )
}
