/* globals up */
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

    playAudio.addEventListener("click", event => {
      audioTrack.play();
    });

    pauseAudio.addEventListener("click", event => {
      audioTrack.pause();
    });

    volumeUp.addEventListener("click", event => {
      var currentVolume = Math.floor(audioTrack.volume * 10) / 10;
      audioTrack.volume = Math.min(currentVolume + 0.1, 1.00);
      audioTrack.muted = false;
    });

    volumeDown.addEventListener("click", event => {
      var currentVolume = Math.floor(audioTrack.volume * 10) / 10;
      audioTrack.volume = Math.max(0.00, currentVolume - 0.1)
      if (audioTrack.volume < 0.05) audioTrack.muted = true;
    });
  }

  up.compiler("#audio-container", function() {
    document
      .querySelectorAll(".hide-audio-button")
      .forEach(el => (el.style.display = "none"));
    document
      .querySelectorAll(".escape-button")
      .forEach(el => (el.style.display = "none"));

    // Does the browser actually support the audio element?
    var supportsAudio = !!document.createElement("audio").canPlayType;
    if (supportsAudio) {
      // Check that document is loaded before running.
      var hideAudioButtons = document.querySelectorAll(".hide-audio-button");

      if (hideAudioButtons.length > 0) {
        audioControlsHideButton(hideAudioButtons[0]);
      }
    } else {
      document
        .querySelectorAll(".escape-button")
        .forEach(el => (el.style.display = "block"));
    }
  });
})();
