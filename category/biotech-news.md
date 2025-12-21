---
layout: default
title: 生技醫藥新聞
permalink: /category/biotech-news
---

{% assign page-category = "生技醫藥新聞" %}

# {{ page.title }}

<div class="post-list">
  <ul>
    {% for post in site.categories[page-category] %}
      <li style="margin-bottom: 25px; list-style: none;">
        <span style="color: #666; font-size: 0.9em;">{{ post.date | date: "%Y-%m-%d" }}</span>
        <br>
        <a href="{{ post.url | relative_url }}" style="font-size: 1.4em; font-weight: bold;">{{ post.title }}</a>
        <br>
        <p>{{ post.excerpt | strip_html | truncatewords: 30 }}</p>
        <a href="{{ post.url | relative_url }}" style="font-size: 0.9em;">繼續閱讀...</a>
        <hr style="margin-top: 15px; border: 0; border-top: 1px solid #eee;">
      </li>
    {% endfor %}
  </ul>
  
  {% if site.categories[page-category].size == 0 %}
    <p>目前此分類尚無文章。</p>
  {% endif %}
</div>
