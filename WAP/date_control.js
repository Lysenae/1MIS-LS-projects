document.addEventListener("DOMContentLoaded", onLoad);

function onLoad()
{
  var ctrl = document.getElementsByClassName("controls")
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
    buildControls(ctrl[0]);
  }
}

function buildControls(ctrl)
{
  var times = document.getElementsByTagName("time");
  buildControlsTable(ctrl, times)
}

function buildControlsTable(ctrl, times)
{
  var table = document.createElement("table");
  table.id  = "tmCtrl"
  buildTableHeader(table);
  for(i=0; i<times.length; ++i)
  {
    buildTableRows(table, times[i], i)
  }
  setControlTablePadding(table);
  ctrl.appendChild(table);
}

function buildTableHeader(table)
{
  var th, text;
  var tr   = document.createElement("tr");
  th   = document.createElement("th");
  text = document.createTextNode("ID");
  th.appendChild(text);
  tr.appendChild(th);

  th   = document.createElement("th");
  text = document.createTextNode("Content");
  th.appendChild(text);
  tr.appendChild(th);

  th   = document.createElement("th");
  text = document.createTextNode("Apply");
  th.appendChild(text);
  tr.appendChild(th);

  table.appendChild(tr);
}

function buildTableRows(table, time, idx)
{
  var td, ct;
  var tr = document.createElement("tr");
  tr.id = "tmCtrl" + idx;

  // Time ID
  td   = document.createElement("td");
  ct = document.createTextNode(idx);
  td.appendChild(ct);
  tr.appendChild(td);

  // Time contents
  td   = document.createElement("td");
  ct = document.createElement("textarea");
  ct.setAttribute("rows", 1);
  ct.style.resize   = "horizontal";
  ct.style.padding  = "4px";
  ct.style.width    = "300px";
  ct.style.maxWidth = "600px";
  ct.value = time.innerText;
  td.appendChild(ct);
  tr.appendChild(td);

  // Apply button
  td = document.createElement("td");
  td.setAttribute("align", "center");
  ct = document.createElement("button");
  ct.setAttribute("type", "button");
  ct.value     = idx;
  ct.innerText = "Apply";
  ct.onclick   = onApply;
  td.appendChild(ct);
  tr.appendChild(td);

  table.appendChild(tr);
}

function setControlTablePadding(table)
{
  var ths = table.getElementsByTagName("th");
  var tds = table.getElementsByTagName("td");
  for(i=0; i<ths.length; ++i)
  {
    ths[i].style.padding = "0 30px 0 30px";
  }
  for(i=0; i<tds.length; ++i)
  {
    tds[i].style.padding = "0 45px 0 45px";
  }
}

function onApply(e)
{
  console.info("Apply clicked " + e.srcElement.value);
  var r = document.getElementById("tmCtrl" + e.srcElement.value);
  var c = r.getElementsByTagName("textarea")[0];
}

