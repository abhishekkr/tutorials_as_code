import "../css/app.scss"

import "phoenix_html"

import socket from "./socket"
import Video from "./video"

let videoNode = document.getElementById("video")
Video.init(socket, videoNode)
