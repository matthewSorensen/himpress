(function(document){
    "use strict";
    var attribute = function(div){
	var url = div.dataset.url,
	title = div.dataset.title || url;
	if(url != ''){
	    var link = document.createElement('a');
	    link.setAttribute('href',url);
	    link.setAttribute('target','_Blank');
	    link.innerText = title;
	    div.appendChild(document.createElement('hr'));
	    div.appendChild(link);
	}
    };
    var blocks = document.getElementsByClassName('code');
    for(var i = 0; i < blocks.length; i++) attribute(blocks[i]);
})(document);