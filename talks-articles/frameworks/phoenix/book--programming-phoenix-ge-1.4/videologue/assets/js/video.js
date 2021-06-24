import {Presence} from "phoenix"
import Player from "./player"

let Video = {

  init(socket, element){
    if(!element){ return }
    let playerId = element.getAttribute("data-player-id")
    let videoId = element.getAttribute("data-id")
    socket.connect()
    Player.init(element.id, playerId, () => {
        this.onReady(videoId, socket)
      })
  },

  onReady(videoId, socket){
    const maxOf = (accumulator, currentValue) => accumulator > currentValue.id ? accumulator : currentValue.id
    let msgContainer = document.getElementById("msg-container")
    let msgInput = document.getElementById("msg-input")
    let postButton = document.getElementById("msg-submit")
    let userList = document.getElementById("user-list")
    let lastSeenId = 0
    //let vidChannel = socket.channel("videos:" + videoId) //before adding last_seen_id
    let vidChannel = socket.channel("videos:" + videoId, () => {
        return {last_seen_id: lastSeenId}
      })

    let presence = new Presence(vidChannel)
    presence.onSync(() => {
        userList.innerHTML = presence.list(
          (id, {user: user, metas: [first, ...rest]}) => {
              let count = rest.length + 1
              return `<span>${user.username} (${count})</span>`
            }).join(", ")
      })

    //make annotations seek video moments
    msgContainer.addEventListener("click", e => {
        e.preventDefault()
        let seconds = e.target.getAttribute("data-seek") ||
                      e.target.parentNode.getAttribute("data-seek")
        if(!seconds){ return }
        Player.seekTo(seconds)
      })

    //join the channel
    vidChannel.join()
      .receive("ok", ( resp ) => {
        console.log("joined video channel", resp)
        window.blah = resp.annotations
        lastSeenId = resp.annotations.reduce(maxOf, 0)
        console.log("lastSeenId", lastSeenId)
        this.scheduleMessages(msgContainer, resp.annotations)
      })
      .receive("error", reason => console.log("join failed", reason))

    vidChannel.on("ping", ({count}) => console.log("PING", count))

    vidChannel.on("new_annotation", (resp) => {
        lastSeenId = resp.id
        this.renderAnnotation(msgContainer, resp)
      })

    postButton.addEventListener("click", (e) => {
        let payload = {body: msgInput.value, at: Player.getCurrentTime()}
        vidChannel.push("new_annotation", payload)
                  .receive("error", (e) => console.log(e))
        msgInput.value = ""
      })
  },

  esc(str){
    let div = document.createElement("div")
    div.appendChild(document.createTextNode(str))
    return div.innerHTML
  },

  renderAnnotation(msgContainer, {user, body, at}){
    let template = document.createElement("div")

    template.innerHTML = `
    <a href="#" data-seek="${this.esc(at)}">
      [${this.formatTime(at)}]
      <b>${this.esc(user.username)}</b>: ${this.esc(body)}
    </a>
    `

    msgContainer.appendChild(template)
    msgContainer.scrollTop = msgContainer.scrollHeight
  },
  formatTime(at){
    let date = new Date(null)
    date.setSeconds(at/1000)
    return date.toISOString().substr(14, 5)
  },

  scheduleMessages(msgContainer, annotations){
    clearTimeout(this.schedulerTimer)
    this.schedulerTimer = setTimeout(() => {
        let videoAtTime = Player.getCurrentTime()
        let annotationsToPost = this.renderAtTime(annotations, videoAtTime, msgContainer)
        this.scheduleMessages(msgContainer, annotationsToPost)
      },
      1000)
  },
  renderAtTime(annotations, videoAt, msgContainer){
    return annotations.filter(ann => {
        if (ann.at > videoAt){
          console.log("not yet", ann)
          return true
        } else {
          console.log(ann.at, "yet", ann)
          this.renderAnnotation(msgContainer, ann)
          return false
        }
      })
  }
}

export default Video
