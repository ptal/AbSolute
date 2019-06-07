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

function readTimeStep(analyse) {
  //var json = JSON.stringify(jsonString);
  document.querySelector("h2").innerHTML = "";
  var name = analyse.name;
  var timeout = analyse.timeout;
  var instances = analyse.instances;
  var inner = '';
  document.querySelector("h1").textContent = name;
  var all = [instances.length];
  for (var j = 0; j < instances.length; j++){
    var inst = instances[j];
    var p_name = inst.problem;
    var i_name = inst.instance;
    var time = inst.time;
    var nb_step = time.length; 
    var nb_instances = inst.nb_instances;
    var colors = ["#ff3333","#ff6c33","#3e95cd","#ffdc33","#a6ff33","#33ff86","#33e6ff","#3353ff","#7633ff","#e233ff"];
    //var h2 = document.querySelector("h2");
    //h2.textContent = "Number of instances solved over "+max_time+" seconds";
    var chart_name = "myChart"+j;
    var div_name = "div_"+chart_name;
    div.push('div_myChart'+j);
    inner = inner + '<div id = "'+div_name+'">'+"Number of instances solved among "+nb_instances+ " over "+timeout+" secondes with prolem "+p_name+" and instance "+i_name+'<canvas id="'+chart_name+'"" width="16px" height="9px"></canvas></div>';
    var database = [nb_step];
    var strategies = inst.strategies;
    for (var i = 0; i < strategies.length; i++){
      var strat = strategies[i];
      database[i] = {data: strat.steps, label: strat.solver+" with "+strat.strategy, borderColor: colors[i%colors.length], fill: false};
    }
    all[j] = database;
   }
   document.querySelector("h2").innerHTML += inner;
   for (var i = 0; i < instances.length; i++){
    var id = "myChart"+i;
    var ctx = document.getElementById(id);
    console.log(ctx ===null);
    var myChart = new Chart(ctx, {
      type: 'line',
      data: {
        labels: instances[i].time,
        datasets: all[i]
      },
      options: {
        title: {
          display: true,
          text: 'Problem : '+instances[i].problem+' - Instance set : '+instances[i].instance+' - Number of instances : '+instances[i].nb_instances+' - Timeout : '+instances[i].time[(instances[i].time.length)-1],
          position: 'top',
          fontSize: 20
        },
        scales: {
          xAxes :[{
            display: true,
            ticks: {
              fontSize: 15
            }
          }],
          yAxes :[{
            display: true,
            ticks: {
              suggestedMax: nb_instances ,
              beginAtZero: true,
              fontSize: 15
            }
          }]
        },
        elements: {
          line: {
            tension: 0
          }
        },
        legend: {
          labels: { fontSize: 20}
       }
      }
    });
  }
}

function readInclusion(analyse){
  //empty_div();
  document.querySelector("h2").innerHTML = "";
  var name = analyse.name;
  var timeout= analyse.timeout;
  var instances = analyse.instances;
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
    inner = inner + '<div id = "'+div_name+'">'+"Instances inclusion between "+solver1+" with "+strat1+" and "+solver2+" with "+strat2+" on problem "+p_name+" and instance "+i_name+'<canvas id="'+chart_name+'"" width="800" height="450"></canvas></div>';
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
                  position: 'top',
                  fontSize: 20
              },
        legend: {
          labels: { fontSize: 20}
       },
        rotation: -0.7 * Math.PI
}
    });
  }
}

function readCactus(analyse){
    document.querySelector("h2").innerHTML = "";
  var name = analyse.name;
  var instances = analyse.instances;
  var timeout = analyse.timeout;
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
    inner = inner + '<div id = "'+div_name+'">'+solver1+" with "+strat1+" vs "+solver2+" with "+ strat2+" on problem "+p_name+" and instance "+i_name+'<canvas id="'+chart_name+'"" width="1600" height="900"></canvas></div>';
  }
  document.querySelector("h2").innerHTML += inner;
  for (var i = 0; i < instances.length; i++){
    var inst = instances[i];
    var cactusOptions = {
        responsive: true,
        title: {
          display: true,
          text: 'Problem : '+instances[i].problem+' - Instance set : '+instances[i].instance+' - Number of instances : '+inst.points.length+' - Timeout : '+timeout,
          position: 'top',
          fontSize: 20
        },
        scales: {
          xAxes: [{
            display: true,
            scaleLabel:{
              display: true,
              labelString: inst.solver1 /*+' with '+inst.strat1*/,
              fontSize: 15
            },
            ticks: {
              min: 0,
              max: timeout,
              fontSize: 15,
              major: {
                fontStyle: 'bold',
                fontColor: '#FF0000'
              }
            },
          }],
          yAxes: [{
            display: true,  
            scaleLabel:{
              display: true,
              labelString: inst.solver2 /*+' with '+inst.strat2*/,
              fontSize: 15
            },
            ticks: {
              min: 0,
              max: timeout,
              fontSize: 15,
              major: {
                fontStyle: 'bold',
                fontColor: '#FF0000'
              }
            }
          }]
        },
        legend: {
          labels: { fontSize: 20}
       }
      }; 
    var id = "myChart"+i;
    var ctx = document.getElementById(id);
    var myChart = new Chart(ctx, {
    type: 'bubble',
    data: {
    datasets: [{type: 'line', 
      label: 'diag',
      data: [{x:0,y:0},{x:timeout,y:timeout}],
      options: cactusOptions,  
      backgroundColor: 'Red',
      fill: false,
      borderColor:'Red',
      },{
      label: 'time',
      data: instances[i].points,
      fill: false, 
      backgroundColor: 'Grey',   
    }]
    },
    options: cactusOptions
  });
  }
}

function readJson(json) {
  var obj = JSON.parse(json);
  var analyses = obj.analyses;
  var inner = "";
  for (var i = 0; i < analyses.length ; i ++) {
    var button_name = "button_"+i;
    var button = '<button type="button" id="'+button_name+'">'+analyses[i].name+'</button>';
    inner = inner + button;
  }
  document.getElementById("buttons").innerHTML += inner;
  document.getElementById("button_"+0).addEventListener("click", function(){readInclusion(analyses[0])}); 
  document.getElementById("button_"+1).addEventListener("click", function(){readCactus(analyses[1])});
  document.getElementById("button_"+2).addEventListener("click", function(){readTimeStep(analyses[2])});

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


