"pickFrom" <-
    function (vec, nsets = 1, return.indices = FALSE, setlabels = NULL, 
              title = "Subset picker", items.label = "Pick from:",
              list.height = 20, items.scrollbar = TRUE)
{
    if (!is.vector(vec)) 
        stop("argument `vec' muct be a vector")
    else vec <- as.character(vec)

    base <- tktoplevel(takefocus = 1)
    tkwm.title(base, title)
    tkwm.geometry(base, "+150+30")
    tkwm.resizable(base, 0, 0)

    right.frm <- tkframe(base)
    left.frm <- tkframe(base)

    items.label <- tklabel(left.frm, text = items.label)
    items.list <- as.character(tclVar(paste("{", 
                                 paste(vec, collapse = "} {"), "}",
                                 sep = "")))
    items.frm <- tkframe(left.frm)
    items.height <- min(list.height, length(vec))
    items <- tklistbox(items.frm, listvar = items.list,
                                 bg = "darkviolet", 
                                 selectmode = "extended", fg = "white",
                                 height = items.height)
    tkgrid(items, row = 0, column = 0, sticky = "ns")
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
    tkgrid(items.scrollbar, row = 0, column = 1, sticky = "ns")
    }
    tkpack(items.label, items.frm,
           pady = 10, padx = 5, side = "top", 
           expand = "true", anchor = "nw")

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
    subset.height <- min(list.height - 5, length(vec))
    for (i in 1:nsets) {
        tkset[[i]] <- tclVar("")
        TCLlabel[[i]] <- tclVar(setlabels[[i]])
        setframe[[i]] <- tkframe(sets.frm, width = 250, relief = "groove", 
                                 borderwidth = 2, bg = "lightgrey")
        label[[i]] <- tklabel(setframe[[i]], text = setlabels[[i]], 
                              bg = "lightgrey")
        listvarname[[i]] <- as.character(tkset[[i]])
        listbox[[i]] <- tklistbox(setframe[[i]],
                                  listvar = listvarname[[i]], 
                                  bg = "white", height = subset.height,
                                  selectmode = "extended")
        labelbox[[i]] <- tkframe(setframe[[i]], width = 250, 
                                 bg = "lightgrey")
        setlabeltext[[i]] <- tklabel(labelbox[[i]],
                                     text = "Label for this set:", 
                                     bg = "lightgrey")
    }
    string.to.vector <- function(string.of.indices) {
        as.numeric(strsplit(string.of.indices, split = " ")[[1]])
    }
    add.cmd <- deparse(function() {
        set[[ppp]] <- match(Tcl.to.R(tclvalue(tkset[[ppp]])), 
                            vec)
        set[[ppp]] <- sort(union(set[[ppp]],
                           1 + string.to.vector(tclvalue(
                                                  tkcurselection(items)))))
        tclvalue(tkset[[ppp]]) <- R.to.Tcl(vec[set[[ppp]]])
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
                                 width = 10, state = "disabled",
                                 command = eval(parse(text = gsub("ppp", 
                                                   as.character(i), add.cmd))))
        remove.but[[i]] <- tkbutton(setframe[[i]], text = "Remove", 
                                    fg = "darkred",
                                    disabledforeground = "darkgrey", 
                                    width = 10, state = "disabled",
                                    command = eval(parse(text = gsub("ppp", 
                                                as.character(i), remove.cmd))))
        labelentry[[i]] <- tkentry(labelbox[[i]],
                                   textvariable = as.character(TCLlabel[[i]]), 
                                   bg = "darkorange", fg = "white")
        tkpack(setlabeltext[[i]], labelentry[[i]], side = "top", 
               anchor = "w")
        tkpack(label[[i]], add.but[[i]], remove.but[[i]], listbox[[i]], 
               labelbox[[i]], side = "top", padx = 5, pady = 5)
        tkpack(setframe[[i]], side = "left", padx = 3, pady = 10)
    }
    fun <- deparse(function() {
        if (tclvalue(tkcurselection(listbox[[ppp]])) != "") {
            for (j in 1:nsets) {
                tkconfigure(add.but[[j]], state = "disabled")
            }
            tkconfigure(remove.but[[ppp]], state = "normal")
        }
        for (j in (1:nsets)[-ppp]) {
            tkconfigure(remove.but[[j]], state = "disabled")
        }
    })
    for (i in 1:nsets) {
        tkbind(listbox[[i]], "<<ListboxSelect>>",
               eval(parse(text = gsub("ppp", as.character(i), fun))))
    }
    tkbind(items, "<<ListboxSelect>>", function() {
        items.selected <- vec[1 + string.to.vector(tclvalue(
                                                 tkcurselection(items)))]
        for (i in 1:nsets) {
            set[[i]] <- Tcl.to.R(tclvalue(tkset[[i]]))
            if (setequal(items.selected, intersect(items.selected, 
                                                   set[[i]]))) {
                tkconfigure(add.but[[i]], state = "disabled")
            }
            else tkconfigure(add.but[[i]], state = "normal")
        }
    })
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
            match(Tcl.to.R(tclvalue(set)), vec)
        })
        labels <- lapply(TCLlabel, tclvalue)
        names(sets) <- labels
        result <- sets
    }
    else result <- NULL
    if (return.indices) 
        return(result)
    else return(lapply(result, function(set) {
        vec[set]
    }))
}
