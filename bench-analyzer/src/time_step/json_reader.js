    function readJSON(path) {
        var xhr = new XMLHttpRequest();
        xhr.open('GET', path, true);
        xhr.responseType = 'blob';
        xhr.onload = function(e) { 
          if (this.status == 200) {
              var file = new File([this.response], 'temp');
              var fileReader = new FileReader();
              fileReader.addEventListener('load', function(){
                  document.log(fileReader.result);
              });
              fileReader.readAsText(file);
          } 
        }
        xhr.send();
    }

readJSON("diane/home/Documents/AbSolute/beanch-analyser/src/time_step/data/data_test.json");