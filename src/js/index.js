import ace from "brace";
import katex from "katex";
import React from "react";
import AceEditor from "react-ace";
import ReactDOM from "react-dom";
import Measure from 'react-measure';
import "brace/keybinding/vim";
import "brace/mode/latex";
import "brace/theme/terminal";
import "katex/dist/katex.css"

window.ace = ace;
window.AceEditor = AceEditor;
window.katex = katex;
window.React = React;
window.ReactDOM = ReactDOM;
window.Measure = Measure;
