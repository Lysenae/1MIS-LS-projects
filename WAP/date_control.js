// Project: WAP - Časové údaje v dokumentu
// Author:  Daniel Klimaj (xklima22@stud.fit.vutbr.cz)

// Global constants
var TM_YEAR   = 31536000000;
var TM_DAY    = 86400000;
var TM_HOUR   = 3600000;
var TM_MINUTE = 60000;
var CANVAS_H  = 201;
var CANVAS_Y  = Math.floor((CANVAS_H - 1) / 2 + 1);
var CANVAS_B  = 40;
var CANVAS_W  = 800 + 2 * CANVAS_B;

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
  {
    buildControls(ctrl[0]);
    updateTimeline();
  }
}

function buildControls(ctrl)
{
  var times = document.getElementsByTagName("time");
  for(i=0; i<times.length; ++i)
  {
    times[i].setAttribute("tm-rel", false);
    times[i].setAttribute("tm-abs", times[i].innerText);
  }
  buildControlsTable(ctrl, times)
  buildTimeline(ctrl);
}

function buildTimeline(ctrl)
{
  var canvas    = document.createElement("canvas");
  canvas.id     = "timeline";
  canvas.width  = CANVAS_W;
  canvas.height = CANVAS_H;
  ctrl.appendChild(canvas);
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
    time.setAttribute("tm-abs", text.value);
    time.title = title.value;
    if(rel.checked)
      updateRelativeTimes();
    else
      time.innerText = text.value;
    updateTimeline();
  }
  else
  {
    text.value  = time.getAttribute("tm-abs");
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
    return "in " + amount + " " + unit + ((amount > 1) ? "s" : "");
  else
    return amount + " " + unit + ((amount > 1) ? "s" : "") + " ago";
}

function updateTimeline()
{
  var times = document.getElementsByTagName("time");
  var vals = [];
  for(i=0; i<times.length; ++i)
    vals.push([times[i].getAttribute("datetime"), times[i].title]);
  vals = vals.sort(cmpTime);
  drawTimeline(vals);
}

function cmpTime(t1, t2)
{
   if (t1[0] < t2[0]) return -1;
   if (t1[0] > t2[0]) return 1;
   return 0;
}

function drawTimeline(times)
{
  var canvas = document.getElementById("timeline");
  var ctx    = canvas.getContext("2d");
  var time, diff, x;

  ctx.clearRect(0, 0, CANVAS_W, CANVAS_H);
  ctx.setLineDash([])
  ctx.beginPath();
  ctx.moveTo(CANVAS_B, CANVAS_Y);
  ctx.lineTo(CANVAS_W-CANVAS_B, CANVAS_Y);
  ctx.stroke();

  var first  = new Date(times[0][0]).getTime();
  var last   = new Date(times[times.length-1][0]).getTime();
  var length = last - first;
  for(i=0; i<times.length; ++i)
  {
    if(i == 0) // Left most
      drawTime(ctx, times[i][1], i, CANVAS_B, CANVAS_Y);
    else if(i == times.length-1) // Right most
      drawTime(ctx, times[i][1], i, CANVAS_W-CANVAS_B, CANVAS_Y);
    else // In between, percentual distance from the most left
    {
      time = new Date(times[i][0]).getTime();
      diff = time - first;
      x = Math.floor((CANVAS_W-(2*CANVAS_B)) * (diff / length)) + CANVAS_B;
      drawTime(ctx, times[i][1], i, x, CANVAS_Y);
    }
  }
}

function drawTime(ctx, title, idx, x, y)
{
  drawDot(ctx, x, y);
  drawPointer(ctx, x, y, (idx % 2 == 0));
  drawTitle(ctx, title, x, y, (idx % 2 == 0));
}

function drawDot(ctx, x, y)
{
  ctx.beginPath();
  ctx.arc(x, y, 3, 0, 2 * Math.PI, false);
  ctx.fill();
  ctx.stroke();
}

function drawPointer(ctx, x, y, above)
{
  ctx.beginPath();
  ctx.setLineDash([4, 1]);
  ctx.moveTo(x, y);
  ctx.lineTo(x, above ? y+25 : y-25);
  ctx.stroke();
}

function drawTitle(ctx, title, x, y, above)
{
  ctx.font = "10px Arial";
  ctx.fillText(title, x, above ? y+35 : y-25, 40);
}
