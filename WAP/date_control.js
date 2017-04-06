// Project: WAP - Časové údaje v dokumentu
// Author:  Daniel Klimaj (xklima22@stud.fit.vutbr.cz)

// Global constants
var TM_YEAR   = 31536000000;
var TM_DAY    = 86400000;
var TM_HOUR   = 3600000;
var TM_MINUTE = 60000;

document.addEventListener("DOMContentLoaded", onLoad);

setInterval(updateRelativeTimes, TM_MINUTE/4);
updateRelativeTimes();

function onLoad()
{
  var ctrl = document.getElementsByClassName("controls")
  if(ctrl.length == 0)
    console.error("Controls not found!");
  else if(ctrl.length > 1)
    console.error("Multiple controls found");
  else
    buildControls(ctrl[0]);
}

function buildControls(ctrl)
{
  var times = document.getElementsByTagName("time");
  for(i=0; i<times.length; ++i)
  {
    times[i].setAttribute("tm-rel", false);
  }
  buildControlsTable(ctrl, times)
}

function buildControlsTable(ctrl, times)
{
  var table = document.createElement("table");
  table.id  = "tmCtrlTbl"
  buildTableHeader(table);
  for(i=0; i<times.length; ++i)
    buildTableRows(table, times[i], i)
  setControlTablePadding(table);
  ctrl.appendChild(table);
}

function buildTableHeader(table)
{
  var th, text;
  var tr = document.createElement("tr");

  th   = document.createElement("th");
  text = document.createTextNode("Order");
  th.appendChild(text);
  tr.appendChild(th);

  th   = document.createElement("th");
  text = document.createTextNode("Time");
  th.appendChild(text);
  tr.appendChild(th);

  th   = document.createElement("th");
  text = document.createTextNode("Title");
  th.appendChild(text);
  tr.appendChild(th);

  th   = document.createElement("th");
  text = document.createTextNode("Relative?");
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
  tr.id  = "tmCtrl" + idx;

  // Time ID
  td   = document.createElement("td");
  ct = document.createTextNode(idx);
  td.appendChild(ct);
  tr.appendChild(td);

  // Time
  td = document.createElement("td");
  ct = buildTextArea("tmCtrlTime" + idx)
  ct.value = time.innerText;
  td.appendChild(ct);
  tr.appendChild(td);

  // Title
  td = document.createElement("td");
  ct = buildTextArea("tmCtrlTitle" + idx)
  ct.value = time.title;
  td.appendChild(ct);
  tr.appendChild(td);

  // Show relative time
  td = document.createElement("td");
  ct = document.createElement("input");
  ct.setAttribute("type", "checkbox");
  ct.id = "tmCtrlRel" + idx;
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

function buildTextArea(id)
{
  ct = document.createElement("textarea");
  ct.setAttribute("rows", 1);
  ct.style.resize   = "horizontal";
  ct.style.padding  = "4px";
  ct.style.width    = "200px";
  ct.style.maxWidth = "400px";
  ct.id             = id;
  return ct;
}

function setControlTablePadding(table)
{
  var ths = table.getElementsByTagName("th");
  var tds = table.getElementsByTagName("td");
  for(i=0; i<ths.length; ++i) ths[i].style.padding = "0 30px";
  for(i=0; i<tds.length; ++i) tds[i].style.padding = "0 45px";
}

function onApply(e)
{
  var idx   = e.srcElement.value
  var text  = document.getElementById("tmCtrlTime" + idx);
  var title = document.getElementById("tmCtrlTitle" + idx);
  var rel   = document.getElementById("tmCtrlRel" + idx);
  var time  = document.getElementsByTagName("time")[parseInt(idx, 10)];
  var rslt  = parseDate(text.value);
  if(rslt != null)
  {
    time.setAttribute("datetime", rslt);
    time.setAttribute("tm-rel", rel.checked);
    time.title = title.value;
    if(rel.checked)
      updateRelativeTimes();
    else
      time.innerText = text.value;
  }
  else
  {
    text.value  = time.innerText;
    title.value = time.title;
  }
}

function parseDate(date)
{
  var t = new Date(date);
  if(isNaN(t.getTime()))
    console.error("Failed to parse date " + date);
  else
    return createDateTime(t);
}

function createDateTime(t)
{
  return t.getFullYear() + "-" + zpad((t.getMonth()+1)) + "-" +
    zpad(t.getDate()) + " " + zpad(t.getHours()) + ":" + zpad(t.getMinutes()) +
    ":" + zpad(t.getSeconds());
}

function zpad(str)
{
  return ("0" + str).slice(-2);
}

function updateRelativeTimes()
{
  var times = document.getElementsByTagName("time");
  var rel, tm, now, diff, n, txt;
  for(i=0; i<times.length; ++i)
  {
    rel = times[i].getAttribute("tm-rel");
    if(rel == "false") continue;
    tm   = new Date(times[i].getAttribute("datetime")).getTime();
    now  = Date.now();
    diff = (now - tm);
    if(diff >= 0)
      txt = getRelativeTime(diff);
    else
      txt = getRelativeTime(tm-now, true);
    times[i].innerText = txt;
  }
}

function getRelativeTime(diff, future = false)
{
  var x, txt;
  if(diff >= TM_YEAR) // More than a year
  {
    x   = Math.floor(diff / TM_YEAR);
    txt = createRelativeTime(x, "year", future);
  }
  else if(diff >= TM_DAY) // More than a day
  {
    x   = Math.floor(diff / TM_DAY);
    txt = createRelativeTime(x, "day", future);
  }
  else if(diff >= TM_HOUR) // More than a hour
  {
    x   = Math.floor(diff / TM_HOUR);
    txt = createRelativeTime(x, "hour", future);
  }
  else if(diff >= TM_MINUTE) // More than a minute
  {
    x   = Math.floor(diff / TM_MINUTE);
    txt = createRelativeTime(x, "minute", future);
  }
  else
    txt = createRelativeTime(Math.floor(diff/1000), "second", future);
  return txt;
}

function createRelativeTime(amount, unit, future = false)
{
  if(future)
  {
    return "in " + amount + " " + unit + ((amount > 1) ? "s" : "");
  }
  else
  {
    return amount + " " + unit + ((amount > 1) ? "s" : "") + " ago";
  }
}
