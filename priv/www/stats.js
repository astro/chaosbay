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

function loadBytes(cb) {
    $.ajax({ url: "/stats/bytes.json",
	 dataType: 'json',
	 success: function(data) {
	     $("#bytes").before("<h2>Swarm Throughput</h2>");
	     $.plot($("#bytes"), [{
		 label: "Download",
	         data: data.down.map(toMB),
		 color: 'green',
		 lines: { fill: true },
		 hoverable: true,
		 clickable: true
	     }, {
		 label: "Upload",
		 data: data.up.map(toMB),
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
		height: 300
            });
	    cb();
	 },
	 error: cb
       });
}
function loadPeers(cb) {
    $.ajax({ url: "/stats/peers.json",
	 dataType: 'json',
	 success: function(data) {
	     $("#peers").before("<h2>Tracked Peers</h2>");
	     $.plot($("#peers"), [{
		 label: "Leechers (IPv4)",
	         data: data.leechers4,
		 color: '#cf0000'
	     }, {
		 label: "Seeders (IPv4)",
	         data: data.seeders4,
		 color: '#007f00'
	     }, {
		 label: "Leechers (IPv6)",
	         data: data.leechers6,
		 color: '#ff6f3f'
	     }, {
		 label: "Seeders (IPv6)",
	         data: data.seeders6,
		 color: '#3fff3f'
	     }], {
                yaxis: { min: 0 },
                xaxis: { mode: 'time',
			 timeformat: "%H:%M"
		       },
		width: 800,
		height: 300
            });
	     if (cb)
		 cb();
	 },
	 error: cb
       });
}

loadBytes(loadPeers);
