---
layout: default
title: 首頁
---

# 歡不專業學術閒聊

生科社畜的科學閒聊<br/>
🧬 Mol Bio｜🧫 Cell Bio｜🦠 Virology

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
