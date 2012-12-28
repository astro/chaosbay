/*function humanSize(size) {
    var units = ['T', 'G', 'M', 'K'];
    var unit = '';
    while(size >= 1024 && units.length > 0) {
        size /= 1024;
        unit = units.pop();
    }
    return (Math.round(size * 10) / 10) + ' ' +
        unit + 'B';
}*/

function formatBandwidth(size) {
    return size+"MB/s";
}

function toMB(values) {
    return [values[0], values[1] / 1024 / 1024];
}

var tzOffset = - new Date().getTimezoneOffset() * 60 * 1000;
function applyTZOffset(values) {
    return [values[0] + tzOffset, values[1]];
}

function loadBytes(cb) {
    $("#bytes").html("<img src='/static/spinner.gif'/>");
    $.ajax({ url: "/stats/bytes.json",
	 dataType: 'json',
	 success: function(data) {
	     $("#bytes").before("<h2>Swarm Throughput</h2>");
	     $.plot($("#bytes"), [{
		 label: "Download",
	         data: data.down.map(toMB).map(applyTZOffset),
		 color: 'green',
		 lines: { fill: true },
		 hoverable: true,
		 clickable: true
	     }, {
		 label: "Upload",
		 data: data.up.map(toMB).map(applyTZOffset),
		 color: 'red',
		 lines: { fill: true },
		 hoverable: true,
		 clickable: true
	     }], {
                yaxis: { min: 0,
			 tickFormatter: formatBandwidth
		       },
                xaxis: { mode: 'time',
			 timeformat: "%H:%M"
		       },
		width: 800,
		height: 300,
		legend: { position: 'nw' }
            });
	    cb();
	 },
	 error: cb
       });
}
function loadPeers(cb) {
    $("#peers").html("<img src='/static/spinner.gif'/>");
    $.ajax({ url: "/stats/peers.json",
	 dataType: 'json',
	 success: function(data) {
	     $("#peers").before("<h2>Tracked Peers</h2>");
	     $.plot($("#peers"), [{
		 label: "Leechers",
	         data: data.leechers4.map(applyTZOffset),
		 color: '#cf0000'
	     }, {
		 label: "Seeders",
	         data: data.seeders4.map(applyTZOffset),
		 color: '#007f00'
	     }, {
		 label: "Leechers (IPv6)",
	         data: data.leechers6.map(applyTZOffset),
		 color: '#ff6f3f'
	     }, {
		 label: "Seeders (IPv6)",
	         data: data.seeders6.map(applyTZOffset),
		 color: '#3fff3f'
	     }], {
                yaxis: { min: 0 },
                xaxis: { mode: 'time',
			 timeformat: "%H:%M"
		       },
		width: 800,
		height: 300,
		legend: { position: 'nw' }
            });
	     if (cb)
		 cb();
	 },
	 error: cb
       });
}

loadBytes(loadPeers);
