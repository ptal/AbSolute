var div = []

function empty_div() {
  while(div.length > 0) {
    var id = div.pop();
    console.log("remove "+id);
    var elem = document.getElementById(id);
    console.log(elem);
    elem.parentNode.removeChild(elem);
  }
}

function downloadPDF(id) {
  var button = document.getElementById(id);
  var div = document.getElementById(button.parentNode.id);
  var canvas = div.getElementsByTagName("canvas")[0];
  //creates image
  var ctx = canvas.getContext('2d');
  ctx.webkitImageSmoothingEnabled = true;
  ctx.imageSmoothingEnabled = true;
  ctx.imageSmoothingEnabled = true;
  ctx.imageSmoothingQuality = "high";
  ctx.drawImage(canvas,0,0); //this gives good img quality 

  var canvasImg = canvas.toDataURL("image/png", 1.0); 
  //creates PDF from img
  var doc = new jsPDF('landscape');
  doc.setFontSize(20);
  doc.addImage(canvasImg, 'PNG', 10, 10, 280, 150 );
  doc.save('canvas.pdf');
}

function readJson(json){
  empty_div();
  var obj = JSON.parse(json);
  var name = obj.name;
  var timeout= obj.timeout;
  var instances = obj.instances;
  var inner = '';
  var title = name + " with timeout "+ timeout;
  document.querySelector("h1").textContent = title;
  for(var i = 0; i < instances.length; i++){
    var inst = instances[i];
    var p_name = inst.problem;
    var i_name = inst.instance;
    var solver1 = inst.solver1;
    var solver2 = inst.solver2;
    var strat1 = inst.strat1;
    var strat2 = inst.strat2;
    var points = inst.points;
    var timeout = inst.timeout;
    var chart_name = "myChart"+i;
    var div_name = "div_"+chart_name;
    var button_name = "button_"+chart_name;
    var button = '<button type="button" id="'+button_name+'">Download PDF</button>';
    div.push('div_myChart'+i);
    inner = inner + '<div id = "'+div_name+'">'+"Instances inclusion between "+solver1+" with "+strat1+" and "+solver2+" with "+strat2+" on problem "+p_name+" and instance "+i_name+'<canvas id="'+chart_name+'"" width="800" height="450"></canvas>'+button+'</div>';
  }
  document.querySelector("h2").innerHTML += inner;
  for (var i = 0; i < instances.length; i++){
    var id = "myChart"+i;
    console.log(id);
    var sum = 0;
    for (var j = 0 ; j < instances[i].data.length; j++) {
      sum += instances[i].data[j];
    }
    document.getElementById('button_'+id).addEventListener("click", function(){downloadPDF(this.id)});
    var ctx = document.getElementById(id);
    console.log(ctx ===null);
    var bg;
    if (instances[i].data[2]>= instances[i].data[3]) {
      bg = ['#45c2f7','#aaaaaa','#ff4444','#00fe47'];
    }else{
      bg = ['#45c2f7','#aaaaaa','#00fe47','#ff4444'];
    }
    var myChart = new Chart(ctx, {
    type: 'pie',
    data: {
      labels: instances[i].labels,
      datasets: [{
        data: instances[i].data,
        backgroundColor: bg
      }]
    },
  options: {
        title: {
                  display: true,
                  text: 'Problem : '+instances[i].problem+' - Instance set : '+instances[i].instance+' - Number of instances : '+sum,
                  position: 'top'
              },
        rotation: -0.7 * Math.PI
}
    });
  }
}


function handleFileSelect(evt){
  var files = evt.target.files;
  var file = files[files.length-1];
  var reader = new FileReader();
  reader.onload = function(e) { var text = reader.result; readJson(text);} 
  reader.readAsText(file);

}

var files = document.getElementById('files');
files.addEventListener('change', handleFileSelect,false);