---
layout: default
title: 用 R 玩分析
permalink: /category/data-analysis
---
{% assign page-category = "用 R 玩分析" %}
<div class="post-list">
  <ul>
    {% for post in site.categories[page-category] %}
      <li style="margin-bottom: 25px; list-style: none;">
        <span style="color: #666; font-size: 0.9em;">{{ post.date | date: "%Y-%m-%d" }}</span><br>
        <a href="{{ post.url | relative_url }}" style="font-size: 1.4em; font-weight: bold;">{{ post.title }}</a><br>
        <p>{{ post.excerpt | strip_html | truncatewords: 30 }}</p>
        <a href="{{ post.url | relative_url }}" style="font-size: 0.9em;">繼續閱讀...</a>
        <hr style="margin-top: 15px; border: 0; border-top: 1px solid #eee;">
      </li>
    {% endfor %}
  </ul>
</div>
