PareditView = require './paredit-view'

require("../out/goog/bootstrap/nodejs")
debugger;
require("../out")
paredit = require("../out/paredit/core")



module.exports = paredit
   pareditView: null
  #
   activate: (state) ->
     @pareditView = new PareditView(state.pareditViewState)
     
  #
  # deactivate: ->
  #   @pareditView.destroy()
  #
  # serialize: ->
    # pareditViewState: @pareditView.serialize()
