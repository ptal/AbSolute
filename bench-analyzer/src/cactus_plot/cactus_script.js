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
  var instances = obj.instances;
  var timeout = obj.timeout;
  var inner = '';
  document.querySelector("h1").textContent = name;
  for(var i = 0; i < instances.length; i++){
    var inst = instances[i];
    var p_name = inst.problem;
    var i_name = inst.instance;
    var time = inst.time;
    var solver1 = inst.solver1;
    var solver2 = inst.solver2;
    var strat1 = inst.strat1;
    var strat2 = inst.strat2;
    var points = inst.points;
    var chart_name = "myChart"+i;
    var div_name = "div_"+chart_name;
    div.push('div_myChart'+i);
    inner = inner + '<div id = "'+div_name+'">'+solver1+" with "+strat1+" vs "+solver2+" with "+ strat2+" on problem "+p_name+" and instance "+i_name+'<canvas id="'+chart_name+'"" width="1600" height="900"></canvas> </div>';
  }
  document.querySelector("h2").innerHTML += inner;
  for (var i = 0; i < instances.length; i++){
    var inst = instances[i];
    var id = "myChart"+i;
    console.log(id);
    var ctx = document.getElementById(id);
    console.log(ctx ===null);
    var myChart = new Chart(ctx, {
    type: 'bubble',
    data: {
    //labels: [0,6,12,18,24,30,36,42,48,54,60],
    datasets: [{
      label: 'time',
      data: instances[i].points,
      fill: false, 
      backgroundColor: 'Grey',   
    },
      {                      
      type: 'line', 
      label: 'diag',
      data: [{x:0,y:0},{x:timeout,y:timeout}],  
      backgroundColor: 'Red',
      fill: false,
      borderColor:'Red'
      }],
      options: {
        responsive: true,
        scales: {
          xAxes: [{
            display: true,
            scaleLabel:{
              display: true,
              labelString: 'solveur'
            },
            ticks: {
              min: 0,
              max: timeout,
              major: {
                fontStyle: 'bold',
                fontColor: '#FF0000'
              }
            },
          }],
          yAxes: [{
            display: true,  
            title: inst.solver1+" with "+inst.strat1,
            ticks: {
              min: 0,
              max: timeout,
              major: {
                fontStyle: 'bold',
                fontColor: '#FF0000'
              }
            }
          }]
        }
      }  
    }});
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

//var json = '{"name":"cactus_plot", "instances":[{"problem":"rcpsp","instance":"j30","timeout":60,"solver1":"absolute","solver2":"chuffed","strat1":"Octagon","strat2":"Min","points":[{"x":1.0,"y":3.2},{"x":2.0,"y":3.2},{"x":3.0,"y":3.2}] }, {"problem":"rcpsp-max","instance":"ubo100","timeout":60,"solver1":"absolute2","solver2":"chuffed2","strat1":"Octagon","strat2":"Min","points":[{"x":1.0,"y":4.2},{"x":1.0,"y":5.2},{"x":1.0,"y":6.2}]}]}'
//readJson(json);