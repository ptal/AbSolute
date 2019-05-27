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
    div.push('div_myChart'+i);
    inner = inner + '<div id = "'+div_name+'">'+"Instances inclusion between "+solver1+" with "+strat1+" and "+solver2+" with "+strat2+" on problem "+p_name+" and instance "+i_name+'<canvas id="'+chart_name+'"" width="800" height="450"></canvas> </div>';
  }
  document.querySelector("h2").innerHTML += inner;
  for (var i = 0; i < instances.length; i++){
    var id = "myChart"+i;
    console.log(id);
    var sum = 0;
    for (var j = 0 ; j < instances[i].data.length; j++) {
      sum += instances[i].data[j];
    }
    var ctx = document.getElementById(id);
    console.log(ctx ===null);
    var myChart = new Chart(ctx, {
    type: 'pie',
    data: {
      labels: instances[i].labels,
      datasets: [{
        data: instances[i].data,
        backgroundColor: ['#45c2f7','#aaaaaa','#00fe47','#ff4444']
      }]
    },
  options: {
        title: {
                  display: true,
                  text: 'Number of instances :'+sum,
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