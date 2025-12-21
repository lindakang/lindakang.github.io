---
layout: default
title: 首頁
---

# 歡迎來到不專業學術閒聊

生科社畜的科學閒聊
🧬 Mol Bio｜🧫 Cell Bio｜🦠 Virology

接案、合作：nonproscience@gmail.com
📍文稿：科普文、公司文、產業文等等
📍諮詢：實驗設計、加拿大研究所、生科產業

如果你喜歡我的分享，歡迎賞一杯咖啡贊助者支持。😊

<div style="margin-top: 40px;">
  <h2>最新文章</h2>
  <ul>
    {% for post in site.posts %}
      <li style="margin-bottom: 15px;">
        <span style="color: #666; font-size: 0.9em;">{{ post.date | date: "%Y-%m-%d" }}</span>
        <br>
        <a href="{{ post.url | relative_url }}" style="font-size: 1.2em; font-weight: bold;">{{ post.title }}</a>
        {% if post.categories.size > 0 %}
            <br>
            <small style="color: #7253ed;">[{{ post.categories | join: ', ' }}]</small>
        {% endif %}
      </li>
    {% endfor %}
  </ul>
</div>
