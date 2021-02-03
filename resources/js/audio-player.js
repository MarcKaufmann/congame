(function () {

    // Does the browser actually support the audio element?
    var supportsAudio = !!document.createElement('audio').canPlayType;

    if (supportsAudio) {

        // Check that document is loaded before running.
        document.addEventListener('DOMContentLoaded', (event) => {

            var hideAudioButtons = document.getElementsByClassName('hide-audio-button');

            if (hideAudioButtons.length > 0) {
                var hideAudioButton = hideAudioButtons[0];
                hideAudioButton.style.display = 'none';
            }

            // Obtain handles to main elements
            var audioContainer = document.getElementById('audio-container');
            var audioTrack = document.getElementById('audio-track');
            var audioControls = document.getElementById('audio-controls');

            audioTrack.addEventListener('ended', (event) => {
                hideAudioButton.style.display = 'block';
            });

            console.log({message: "Check audio track", audioTrack: audioTrack});

            var playAudio = document.getElementById('play');
            var pauseAudio = document.getElementById('pause');
            var volumeUp = document.getElementById('volume-up');
            var volumeDown = document.getElementById('volume-down');

            playAudio.addEventListener('click', function(e) {
                // FIXME: The commented out `if` statement does not work, presumably because
                // audio.paused or audio.ended are undefined and then everythign falls apart.
                // In the console, the function however *does* work.
                // congame also runs this file twice, and seems to load audio-player.js directly,
                // although I thought it should only load a single app.js file with *.js files concatenated.
                // if (audio.paused || audio.ended) audio.play();
                // else audio.pause();
                audioTrack.play();
            });

            pauseAudio.addEventListener('click', function(e) {
                audioTrack.pause();
            })

            volumeUp.addEventListener('click', function(e) {
                var currentVolume = Math.floor(audioTrack.volume * 10) / 10;
                audioTrack.volume += 0.05;
                audioTrack.muted = false;
            })

            volumeDown.addEventListener('click', (event) => {
                var currentVolume = Math.floor(audioTrack.volume * 10) / 10;
                if (currentVolume > 0) audioTrack.volume -= 0.05;
                else audioTrack.muted = true;
            })
        });
    }
})();
