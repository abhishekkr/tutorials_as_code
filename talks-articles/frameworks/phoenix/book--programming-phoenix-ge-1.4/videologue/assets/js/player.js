let Player = {
  player: null,

  init(domId, playerId, onReady){
    window.onYouTubeIframeAPIReady = () => {
      this.onIframeReady(domId, playerId, onReady)
    }
    let youtubeScriptTag = document.createElement("script")
    youtubeScriptTag.id = "yt-iframe"
    youtubeScriptTag.src = "//www.youtube.com/iframe_api"
    document.head.appendChild(youtubeScriptTag)
  },
  onIframeReady(domId, playerId, onReady){
    this.player = new YT.Player(domId, {
        height: "360",
        width: "420",
        videoId: playerId,
        events: {
          "onReady": (event => this.onPlayerReady(onReady, event)),
          "onStateChange": this.onPlayerStateChange
        }
      })
  },
  onPlayerReady(onReady, event){
    console.log("ready", event)
    onReady(event)
  },
  onPlayerStateChange(event){
    console.log("state change", event)
  },
  getCurrentTime(){
    return Math.floor(this.player.getCurrentTime() * 1000)
  },
  seekTo(millsec){
    return this.player.seekTo(millsec/1000)
  }
}

export default Player
