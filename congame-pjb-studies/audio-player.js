(function() {
  function audioControlsHideButton(hideAudioButton) {
    hideAudioButton.style.display = "none";

    // Obtain handles to main elements
    var audioContainer = document.getElementById("audio-container");
    var audioTrack = document.getElementById("audio-track");
    var audioControls = document.getElementById("audio-controls");

    audioTrack.addEventListener("ended", event => {
      hideAudioButton.style.display = "block";
    });

    console.log({ message: "Check audio track", audioTrack: audioTrack });

    var playAudio = document.getElementById("play");
    var pauseAudio = document.getElementById("pause");
    var volumeUp = document.getElementById("volume-up");
    var volumeDown = document.getElementById("volume-down");

    playAudio.addEventListener("click", function(e) {
      audioTrack.play();
    });

    pauseAudio.addEventListener("click", function(e) {
      audioTrack.pause();
    });

    volumeUp.addEventListener("click", function(e) {
      var currentVolume = Math.floor(audioTrack.volume * 10) / 10;
      audioTrack.volume += 0.05;
      audioTrack.muted = false;
    });

    volumeDown.addEventListener("click", event => {
      var currentVolume = Math.floor(audioTrack.volume * 10) / 10;
      if (currentVolume > 0) audioTrack.volume -= 0.05;
      else audioTrack.muted = true;
    });
  }

  // Does the browser actually support the audio element?
  var supportsAudio = !!document.createElement("audio").canPlayType;

  if (supportsAudio) {
    // Check that document is loaded before running.
    up.compiler("#audio-container", function() {
      var hideAudioButtons = document.getElementsByClassName(
        "hide-audio-button"
      );

      if (hideAudioButtons.length > 0) {
        audioControlsHideButton(hideAudioButtons[0]);
      }
    });
  }
})();
