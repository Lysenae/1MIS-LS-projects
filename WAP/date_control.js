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
    buildControls(ctrl[0]);
  }
}

function buildControls(ctrl)
{
  console.info("Building controls for '%s'", ctrl);
  var timeElems = document.getElementsByTagName("time");
  console.info("|<time>|: %d", timeElems.length);
  for(i=0; i<timeElems.length; ++i)
  {
    appendControl(ctrl, i)
  }
}

function appendControl(ctrl, idx)
{
  var element = document.createElement("p");
  var text    = document.createTextNode("Time " + idx);
  element.appendChild(text);
  ctrl.appendChild(element);
}

