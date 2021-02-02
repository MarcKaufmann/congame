/*(function () {

    // Does the browser actually support the audio element?
    var supportsAudio = !!document.createElement('audio').canPlayType;

    if (supportsAudio) {
        // Obtain handles to main elements
        var audioContainer = document.getElementById('audioContainer');
        var audio = document.getElementById('audio');
        var audioControls = document.getElementById('audio-controls');


        var playpause = document.getElementById('playpause');
        var volinc = document.getElementById('volume-up');
        var voldec = document.getElementById('volume-down');

        playpause.addEventListener('click', function(e) {
            if (audio.paused || audio.ended) audio.play();
            else audio.pause();
        });

        // Check the volume
        /*
        var checkVolume = function(dir) {
            if (dir) {
                var currentVolume = Math.floor(audio.volume * 10) / 10;
                if (dir === '+') {
                    if (currentVolume < 1) audio.volume += 0.1;
                }
                else if (dir === '-') {
                    if (currentVolume > 0) audio.volume -= 0.1;
                }
                // If the volume has been turned off, also set it as muted
                // Note: can only do this with the custom control set as when the 'volumechange' event is raised, there is no way to know if it was via a volume or a mute change
                if (currentVolume <= 0) audio.muted = true;
                else audio.muted = false;
            }
            changeButtonState('mute');
        }

        // Change the volume
        var alterVolume = function(dir) {
            checkVolume(dir);
        }

        // Only add the events if addEventListener is supported (IE8 and less don't support it, but that will use Flash anyway)
        if (document.addEventListener) {

            // Changes the button state of certain button's so the correct visuals can be displayed with CSS
            var changeButtonState = function(type) {
                // Play/Pause button
                if (type == 'playpause') {
                    if (audio.paused || audio.ended) {
                        playpause.setAttribute('data-state', 'play');
                    }
                    else {
                        playpause.setAttribute('data-state', 'pause');
                    }
                }
            }

            // Add event listeners for video specific events
            audio.addEventListener('play', function() {
                changeButtonState('playpause');
            }, false);
            audio.addEventListener('pause', function() {
                changeButtonState('playpause');
            }, false);
            audio.addEventListener('volumechange', function() {
                checkVolume();
            }, false);

            // Add events for all buttons
            up.on(playpause, 'click', function(e) {
                if (audio.paused || audio.ended) audio.play();
                else audio.pause();
            });

            volinc.addEventListener('click', function(e) {
                alterVolume('+');
            });
            voldec.addEventListener('click', function(e) {
                alterVolume('-');
            });
        }
    }

})();

*/
var audioContainer = document.getElementById('audioContainer');
var audio = document.getElementById('audio');
var audioControls = document.getElementById('audio-controls');


var playpause = document.getElementById('playpause');
var volinc = document.getElementById('volume-up');
var voldec = document.getElementById('volume-down');

playpause.addEventListener('click', function(e) {
    if (audio.paused || audio.ended) audio.play();
    else audio.pause();
});
