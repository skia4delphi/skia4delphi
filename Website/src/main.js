import Vue from "vue";
import App from "./App.vue";
import router from "./router";
import store from "./store";
import vuetify from "./plugins/vuetify";

import axios from "axios";

Vue.config.productionTip = false;

new Vue({
	router,
	store,
	vuetify,
	data: {
		assets: [],
		error: !1,
		clicked: !1,
	},
	methods: {
		download(t) {
			(this.clicked = !1),
				axios
					.get(t)
					.then(({ data }) => (this.assets = data.assets))
					.catch((e) => ((this.error = !0), console.error(e)))
					.finally(() => {
						if (!this.error) {
							this.assets.forEach((i) => {
								if (this.clicked) return;

								if (i.name.toLowerCase().endsWith(".exe")) {
									(this.clicked = !0), window.open(i.browser_download_url);
								}
							});
						}
					});
		},
	},
	render: (h) => h(App),
}).$mount("#app");
