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
  var times = document.getElementsByTagName("time");
  console.info("|<time>|: %d", times.length);
  buildControlsTable(ctrl, times)
}

function buildControlsTable(ctrl, times)
{
  var table = document.createElement("table");
  table.id  = "ctrlTable"
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
  text = document.createTextNode("Show date?");
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
  var td, text, ip;
  var tr = document.createElement("tr");
  tr.className = idx;

  td   = document.createElement("td");
  text = document.createTextNode(idx);
  td.appendChild(text);
  tr.appendChild(td);

  td   = document.createElement("td");
  text = document.createElement("textarea");
  text.setAttribute("rows", 1);
  text.setAttribute("cols", 40);
  text.style.resize  = "horizontal";
  text.style.padding = "4px";
  text.value = time.innerText;
  td.appendChild(text);
  tr.appendChild(td);

  td = document.createElement("td");
  td.setAttribute("align", "center");
  ip = document.createElement("input");
  ip.setAttribute("type", "checkbox");
  td.appendChild(ip);
  tr.appendChild(td);

  td = document.createElement("td");
  td.setAttribute("align", "center");
  ip = document.createElement("input");
  ip.setAttribute("type", "submit");
  ip.value = "Apply";
  td.appendChild(ip);
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
    console.info(i)
    tds[i].style.padding = "0 45px 0 45px";
  }
}

