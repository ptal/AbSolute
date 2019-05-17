function readJson(json) {
  var obj = JSON.parse(json); //instance list
  var db = obj.database;
  var name = db[0].name;
  console.log(name);
  var instances = db[0].instances;
  var inner = '';
  document.querySelector("h1").textContent = name;
  var all = [instances.length];
  for (var j = 0; j < instances.length; j++){
  	  var inst = instances[j];
  	  var p_name = inst.problem;
  	  var i_name = inst.instance;
  	  var time = inst.time;
  	  var nb_step = time.length; 
	  var max_time = time[nb_step-1];
	  var nb_chart = 0;
	  var colors = ["#ff3333","#ff6c33","#3e95cd","#ffdc33","#a6ff33","#33ff86","#33e6ff","#3353ff","#7633ff","#e233ff"];
	  var h2 = document.querySelector("h2");
	  h2.textContent = "Number of instances solved over "+max_time+" seconds";
	  var chart_name = "myChart"+j;
	  var div_name = "div_"+chart_name;
	  console.log(chart_name);
	  inner = inner + '<div id = "'+div_name+'">'+p_name+" with "+i_name+'<canvas id="'+chart_name+'"" width="1600" height="900"></canvas> </div>';
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
	 	console.log(id);
	 	var ctx = document.getElementById(id);
	  console.log(ctx ===null);
	  var myChart = new Chart(ctx, {
	    type: 'line',
	    data: {
	      labels: instances[i].time,
	      datasets: all[i]
	    }
	  });
  }
}
var json0 ='{"name":"rcpsp 10 steps on j30 and patterson","instances":[{"problem":"rcpsp","instance":"j30","time":[6,12,18,24,30,36,42,48,54,60],"strategies":[{"solver":"Absolute-03","strategy":"Octagon(MSLF,all)","steps":[0,0,33,97,141,171,194,208,221,229]},{"solver":"Absolute-03","strategy":"Octagon(Min_max,LB)","steps":[167,172,175,176,177,177,180,180,180,181]},{"solver":"Absolute-03","strategy":"Octagon(Max_min, LB)","steps":[210,213,216,217,218,218,219,219,219,222]},{"solver":"Gecode","strategy":"rcpsp-cumulative-LB","steps":[332,332,334,338,342,343,343,344,344,347]}]},{"problem":"rcpsp","instance":"patterson","time":[6,12,18,24,30,36,42,48,54,60],"strategies":[{"solver":"Absolute-03","strategy":"Octagon(MSLF,all)","steps":[0,0,33,97,141,171,194,208,221,229]},{"solver":"Absolute-03","strategy":"Octagon(Min_max,LB)","steps":[167,172,175,176,177,177,180,180,180,181]}]}]}';

var json = '{"database":[{"name":"Timeout 60 seconds with 10 steps","instances":[{"problem":"rcpsp","instance":"j30","time":[0,6,12,18,24,30,36,42,48,54,60],"strategies":[{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(Max_min,LB)","steps":[0,210,213,216,217,218,218,219,219,219,222]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(Min_max,LB)","steps":[0,167,172,175,176,177,177,180,180,180,181]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(MSLF,all)","steps":[0,0,0,33,97,141,171,194,208,221,229]},{"solver":"chuffed-0.10","strategy":"chuffed(Box(smallest,indomain_min,complete))","steps":[0,470,473,475,475,476,476,476,477,477,477]},{"solver":"gecode-6.0.1","strategy":"gecode(Box(smallest,indomain_min,complete))","steps":[0,296,300,301,303,304,305,305,307,307,307]},{"solver":"gecode-6.0.1","strategy":"gecode(rcpsp-cumulative-LB.mzn)","steps":[0,332,332,334,338,342,343,343,344,344,347]}]},{"problem":"rcpsp","instance":"patterson","time":[0,6,12,18,24,30,36,42,48,54,60],"strategies":[{"solver":"absolute-02-04-787781b86","strategy":"Box(First_fail,LB)","steps":[0,5,5,5,5,5,5,5,5,5,5]},{"solver":"absolute-02-04-787781b86","strategy":"Octagon(First_fail,LB,Canonical)","steps":[0,22,24,26,28,28,29,29,29,29,30]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(First_fail,LB,Canonical)","steps":[0,22,24,26,28,28,28,29,29,29,29]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(Max_min,LB)","steps":[0,20,22,23,23,24,24,25,26,27,27]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(MSLF without tie breaking)","steps":[0,32,34,35,36,36,37,37,37,39,39]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(Min_max,LB)","steps":[0,16,16,16,18,18,18,19,19,19,19]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(MSLF,all)","steps":[0,19,25,25,27,29,31,31,31,31,32]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(MSLF)","steps":[0,30,33,35,36,36,38,38,39,39,40]},{"solver":"chuffed-0.10","strategy":"chuffed(Box(first_fail,indomain_min,complete))","steps":[0,110,110,110,110,110,110,110,110,110,110]},{"solver":"chuffed-0.10","strategy":"chuffed(Box(smallest,indomain_min,complete))","steps":[0,110,110,110,110,110,110,110,110,110,110]},{"solver":"gecode-6.0.1","strategy":"gecode(Box(first_fail,indomain_min,complete))","steps":[0,97,98,99,100,101,102,104,105,105,105]},{"solver":"gecode-6.0.1","strategy":"gecode(Box(smallest,indomain_min,complete))","steps":[0,97,99,100,101,101,101,101,101,101,101]},{"solver":"gecode-6.0.1","strategy":"gecode(rcpsp-cumulative.mzn)","steps":[0,101,101,103,106,106,106,106,106,107,107]},{"solver":"gecode-6.0.1","strategy":"gecode(rcpsp-cumulative-LB.mzn)","steps":[0,99,100,101,102,102,102,103,103,103,104]}]},{"problem":"rcpsp-max","instance":"ubo100","time":[0,6,12,18,24,30,36,42,48,54,60],"strategies":[{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(Max min,bisect)","steps":[0,8,8,10,10,10,10,10,11,11,12]},{"solver":"absolute-03-04-ca3f8b0","strategy":"Octagon(MSLF without tie breaking)","steps":[0,9,10,10,10,10,10,10,11,11,11]},{"solver":"chuffed-0.10","strategy":"chuffed(Box(smallest,indomain_min,complete))","steps":[0,53,58,61,62,63,64,65,65,66,67]},{"solver":"gecode-6.0.1","strategy":"gecode(Box(smallest,indomain_min,complete))","steps":[0,16,16,16,16,16,16,16,16,16,16]},{"solver":"gecode-6.0.1","strategy":"gecode(rcpsp-cumulative-LB.mzn)","steps":[0,28,28,28,28,28,28,28,28,29,29]}]}]}]}';
readJson(json);

