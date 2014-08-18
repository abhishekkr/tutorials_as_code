(function() {

    var streaming   = false,
    video           = document.querySelector('#live'),
    canvas          = document.querySelector('#snapshot'),
    photo           = document.querySelector('#photo'),
    startbutton     = document.querySelector('#startbutton'),
    filmroll        = document.querySelector('#filmroll'),
    width           = 640,
    height          = 0;

    var getMediaOptions = { video: true, audio: false },
    mediaStreamPlayer   = function(stream) {
                            if (navigator.mozGetUserMedia) {
                              video.mozSrcObject = stream;
                            } else {
                              var vendorURL = window.URL || window.webkitURL;
                              video.src = vendorURL.createObjectURL(stream);
                            }
                            video.play();
                          },
    errorManager        = function(err) {
                            console.log("An error occured! " + err);
                          };

    navigator.getMedia  = ( navigator.getUserMedia ||
                            navigator.webkitGetUserMedia ||
                            navigator.mozGetUserMedia ||
                            navigator.msGetUserMedia  );

    navigator.getMedia(
        getMediaOptions,
        mediaStreamPlayer,
        errorManager
    );

    video.addEventListener('canplay', function(ev){
        if (!streaming) {
            height = video.videoHeight / (video.videoWidth/width);
            video.setAttribute('width', width);
            video.setAttribute('height', height);
            canvas.setAttribute('width', width);
            canvas.setAttribute('height', height);
            streaming = true;
        }
    }, false);

    function pushCanvasToFilmroll(){
        var img = document.createElement("img");
        img.src = canvas.toDataURL("image/png");
        img.style.padding = 5;
        img.width = canvas.width / 4;
        img.height = canvas.height / 4;
        filmroll.appendChild(img);
    }

    function takepicture() {
        canvas.width = width;
        canvas.height = height;
        canvas.getContext('2d').drawImage(video, 0, 0, width, height);
        var data = canvas.toDataURL('image/png');
        photo.setAttribute('src', data);
    }

    startbutton.addEventListener('click', function(ev){
        pushCanvasToFilmroll();
        takepicture();
        ev.preventDefault();
    }, false);

})();

