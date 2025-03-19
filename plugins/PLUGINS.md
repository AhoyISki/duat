# Plugins

This directory contains a list of various plugins used by default when 
installing Duat. At the moment, it consists of three plugins:

* [`duat-kak`][__link0] is a plugin that changes the default mode of Duat to one
  inspired by [Kakoune][__link1]'s "Normal", also bringing with it various
  other modes from Kakoune.
* [`duat-catppuccin`][__link2] is a just a simple colorscheme plugin, it adds 
  the four flavors from the [catppuccin][__link3] colorscheme. You can pick 
  between the four of them, you can apply its colors to other [`Form`]s and you 
  can allow or disallow the colorscheme to set the background color. 
* [`duat-treesitter`][__link4] brinks [tree-sitter][__link5] to Duat in the 
  form of syntax highlighting and indentation calculation, which can be used by 
  Modes (such as those from `duat-kak`) in order to give better feedback when 
  editing files.
 
 [__link0]: [https://github.com/AhoyISki/duat-kak]
 [__link1]: [https://github.com/mawww/kakoune]
 [__link2]: [https://github.com/AhoyISki/duat-catppuccin]
 [__link3]: [https://catppuccin.com]
 [__link4]: [https://github.com/AhoyISki/duat-treesitter]
 [__link5]: [https://tree-sitter.github.io/tree-sitter]
