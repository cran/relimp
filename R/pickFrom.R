R.to.Tcl <-
function (character.vector) 
##  converts a character vector into a brace-delimited Tcl list
{
    paste("{", paste(character.vector, collapse = "} {"), "}", 
        sep = "")
}

Tcl.to.R <-
function (tcl.list) 
##  converts a fully brace-delimited Tcl list into a character 
##  vector in R
{
    tcl.list <- substring(tcl.list, 2, nchar(tcl.list) - 1)
    strsplit(tcl.list, split = "} {")[[1]]
}

pickFrom <-
function (vec, nsets = 1, setlabels = NULL, title = "Subset picker", 
    items.label = "Pick from:") 
{
    if (!is.vector(vec)) 
        stop("argument `vec' muct be a vector")
    else vec <- as.character(vec)
    require(tcltk) || stop("tcltk support is absent")
    base <- tktoplevel(takefocus = 1)
    tkwm.title(base, title)
    tkwm.geometry(base, "+150+30")
    tkwm.resizable(base, 0, 0)
    right.frm <- tkframe(base)
    left.frm <- tkframe(base)
    items.label <- tklabel(left.frm, text = items.label)
    items <- tklistbox(left.frm, listvar = as.character(tclVar(paste("{", 
        paste(vec, collapse = "} {"), "}", sep = ""))), bg = "darkviolet", 
        selectmode = "extended", fg = "white", height = min(15, 
            length(vec)))
    tkpack(items.label, items, pady = 10, padx = 5, side = "top", 
        expand = "true", anchor = "n")
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
    for (i in 1:nsets) {
        tkset[[i]] <- tclVar("")
        TCLlabel[[i]] <- tclVar(setlabels[[i]])
        setframe[[i]] <- tkframe(sets.frm, width = 250, relief = "groove", 
            borderwidth = 2, bg = "lightgrey")
        label[[i]] <- tklabel(setframe[[i]], text = setlabels[i], 
            bg = "lightgrey")
        listvarname[[i]] <- as.character(tkset[[i]])
        listbox[[i]] <- tklistbox(setframe[[i]], listvar = listvarname[[i]], 
            bg = "white", height = min(15, length(vec)),
                                  selectmode = "extended")
        labelbox[[i]] <- tkframe(setframe[[i]], width = 250, 
            bg = "lightgrey")
        setlabeltext[[i]] <- tklabel(labelbox[[i]],
                                     text = "Label for this set:", 
            bg = "lightgrey")
    }
    reset.okbutton <- function(tkset) {
        contentLength <- function(set) {
            length(Tcl.to.R(tclvalue(set)))
        }
        lengths <- lapply(tkset, contentLength)
        if (any(lengths == 0)) {
            tkconfigure(ok.but, state = "disabled")
        }
        else tkconfigure(ok.but, state = "normal")
    }
    string.to.vector <-
       function (string.of.indices) 
       ##  converts a string of indices, as returned by tkcurselection(widget),
       ##  into an index vector for use by R
       {
           as.numeric(strsplit(string.of.indices, split = " ")[[1]])
       }
    add.cmd <- deparse(function() {
        set[[ppp]] <- match(Tcl.to.R(tclvalue(tkset[[ppp]])), 
            vec)
        set[[ppp]] <- sort(union(set[[ppp]],
                                 1 + string.to.vector(tkcurselection(items))))
        tclvalue(tkset[[ppp]]) <- R.to.Tcl(vec[set[[ppp]]])
        tkconfigure(add.but[[ppp]], state = "disabled")
        reset.okbutton(tkset)
    })
    remove.cmd <- deparse(function() {
        Rtkset[[ppp]] <- Tcl.to.R(tclvalue(tkset[[ppp]]))
        out <- 1 + string.to.vector(tkcurselection(listbox[[ppp]]))
        tclvalue(tkset[[ppp]]) <- R.to.Tcl(Rtkset[[ppp]][-out])
        tkconfigure(remove.but[[ppp]], state = "disabled")
        tkselection.clear(listbox[[ppp]], "0", "end")
        reset.okbutton(tkset)
    })
    for (i in 1:nsets) {
        add.but[[i]] <- tkbutton(setframe[[i]], text = "Add", 
            fg = "darkgreen", disabledforeground = "darkgrey", 
            width = 10, state = "disabled",
                        command = eval(parse(text = gsub("ppp", 
                                             as.character(i), add.cmd))))
        remove.but[[i]] <- tkbutton(setframe[[i]], text = "Remove", 
            fg = "darkred", disabledforeground = "darkgrey", 
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
        for (j in 1:nsets) {
            tkconfigure(add.but[[j]], state = "disabled")
        }
        if (tkcurselection(listbox[[ppp]]) != "") 
            tkconfigure(remove.but[[ppp]], state = "normal")
        for (j in (1:nsets)[-ppp]) {
            tkconfigure(remove.but[[j]], state = "disabled")
        }
    })
    for (i in 1:nsets) {
        tkbind(listbox[[i]], "<<ListboxSelect>>",
               eval(parse(text = gsub("ppp", 
                          as.character(i), fun))))
    }
    tkbind(items, "<<ListboxSelect>>", function() {
        items.selected <- vec[1 + string.to.vector(tkcurselection(items))]
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
    tkconfigure(ok.but, state = "disabled")
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
    return(result)
}





