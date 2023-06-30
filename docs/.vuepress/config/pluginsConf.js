
const moment = require('moment');

module.exports = {
    'vuepress-plugin-auto-sidebar': {
        // options
        nav: true,
        removeEmptyGroup: true,
        collapse: {
            open: true
        },
        title: {
            mode: "titlecase",
            map: {
                "/200 ABAP/10 基础语法/": "Hello Vuepress",
                "/100 SD/": "Hello 101",
                "/100 SD/102/": "Hello 102"
            }
        }
    },
    '@vuepress/last-updated':
      {
        transformer: (timestamp, lang) => {
          // 不要忘了安装 moment
          // moment.locale(lang)
          moment.locale('zh-cn')
          return moment(timestamp).format('LLLL')
        }
      }
}