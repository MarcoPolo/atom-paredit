{View} = require 'atom'

module.exports =
class PareditView extends View
  @content: ->
    @div class: 'paredit overlay from-top', =>
      @div "The Paredit package is Alive! It's ALIVE!", class: "message"

  initialize: (serializeState) ->
    atom.workspaceView.command "paredit:toggle", => @toggle()

  # Returns an object that can be retrieved when package is activated
  serialize: ->

  # Tear down any state and detach
  destroy: ->
    @detach()

  toggle: ->
    console.log "PareditView was toggled!"
    if @hasParent()
      @detach()
    else
      atom.workspaceView.append(this)
