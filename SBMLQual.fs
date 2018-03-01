module SBMLQual

open Circuit
open FSharpx.Collections

let private circuitToFunctionTerm c =
    let rec toTerm c =
        match c with
        | Value true -> """<cn type="integer"> 1 </cn>"""
        | Value false -> """<cn type="integer"> 0 </cn>"""
        | And (c1, c2) -> sprintf "<apply><and/>%s%s</apply>" (toTerm c1) (toTerm c2)
        | Or (c1, c2) -> sprintf "<apply><or/>%s%s</apply>" (toTerm c1) (toTerm c2)
        | Not c -> sprintf "<apply><not/>%s</apply>" (toTerm c)
        | Node name -> sprintf "<ci> %s </ci>" name
    toTerm c

let private header =
    """<?xml version='1.0' encoding='UTF-8' standalone='no'?>""" + "\n" +
    """<sbml xmlns="http://www.sbml.org/sbml/level3/version1/core" level="3" version="1" xmlns:qual="http://www.sbml.org/sbml/level3/version1/qual/version1" qual:required="true">""" + "\n" +
    """<model id="model">""" + "\n" +
    "<listOfCompartments>\n" +
    """<compartment id="default" constant="true" />""" + "\n" +
    "</listOfCompartments>\n"

let private footer = "</model>\n</sbml>"

let private printSpecies var =
    sprintf """<qual:qualitativeSpecies qual:maxLevel="1" qual:compartment="default" qual:constant="false" qual:id="%s"/>""" var

let private printInputs var inputs =
    let str = inputs |> Seq.mapi (fun i v' -> sprintf """<qual:input qual:qualitativeSpecies="%s" qual:transitionEffect="none" qual:id="tr_%s_in_%i"/>""" v' var i) |> String.concat "\n"
    "<qual:listOfInputs>\n" + str + "\n</qual:listOfInputs>\n"

let private printOutput var =
    "<qual:listOfOutputs>\n" +
    sprintf """<qual:output qual:qualitativeSpecies="%s" qual:transitionEffect="assignmentLevel" qual:id="%s_out"/>""" var var + "\n" +
    "</qual:listOfOutputs>\n"

let private printFunctionTerms c =
    "<qual:listOfFunctionTerms>\n" +
    """<qual:defaultTerm qual:resultLevel="0"></qual:defaultTerm>""" + "\n" +
    """<qual:functionTerm qual:resultLevel="1">""" + "\n" +
    """<math xmlns="http://www.w3.org/1998/Math/MathML">""" + "\n" +
    circuitToFunctionTerm c +
    "\n</math>\n</qual:functionTerm>\n</qual:listOfFunctionTerms>\n"

let private printTransitions (var, c) =
    let openTag = sprintf """<qual:transition qual:id="tr_%s">""" var + "\n"
    let inputs = printInputs var (variables c)
    let output = printOutput var
    let functionTerms = printFunctionTerms c
    let closeTag = "</qual:transition>\n"
    openTag + inputs + output + functionTerms + closeTag

let modelToSbml (model : Map<Gene, Circuit>) =
    let openSpecies = """<qual:listOfQualitativeSpecies xmlns:qual="http://www.sbml.org/sbml/level3/version1/qual/version1">""" + "\n"
    let closeSpecies = """</qual:listOfQualitativeSpecies>""" + "\n"
    let openTransitions = """<qual:listOfTransitions xmlns:qual="http://www.sbml.org/sbml/level3/version1/qual/version1">""" + "\n"
    let closeTransitions = """</qual:listOfTransitions>""" + "\n"
    let species = openSpecies + (Seq.map printSpecies (Map.keys model) |> String.concat "\n") + closeSpecies
    let transitions = openTransitions + (Seq.map printTransitions (Map.toSeq model) |> String.concat "\n") + closeTransitions
    header + species + transitions + footer
