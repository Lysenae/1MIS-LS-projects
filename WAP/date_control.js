document.addEventListener("DOMContentLoaded", onLoad);

function onLoad()
{
  var ctrl = document.getElementsByClassName("controls")
  console.info("Controls: %d", ctrl.length)
  if(ctrl.length == 0)
  {
    console.error("Controls not found!");
  }
  else if(ctrl.length > 1)
  {
    console.error("Multiple controls found");
  }
  else
  {
    console.info("All ok");
    buildControls();
  }
}

function buildControls()
{
  console.info("Building controls");
}

