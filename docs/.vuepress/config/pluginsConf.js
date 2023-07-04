
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
        "/200-ABAP/10-基础语法/": "Hello Vuepress",
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
      // moment.locale('zh-cn')
      // September 4, 1986
      return moment(timestamp).format('LL')
    }
  },
  '@vssue/vuepress-plugin-vssue': {
    // 设置 `platform` 而不是 `api`
    platform: 'github-v4',
    // 其他的 Vssue 配置
    owner: 'ClaudDeng',
    repo: 'iClaud',
    clientId: '61eb248da2c2589810ea',
    clientSecret: '8c2802d2c7483bbc2dfb68cdbf7f5e5faff900fc',

    autoCreateIssue: true,
    //默认每页显示的评论数
    perPage: 5,
    locale: 'zh',
  }
}