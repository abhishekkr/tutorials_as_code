import "../css/app.scss"

import "phoenix_html"

import Player from "./player"
let video = document.getElementById("video")
if (video) {
  let log_ready = () => { console.log("player ready!") }
  Player.init(video.id, video.getAttribute("data-player-id"), log_ready)
}
