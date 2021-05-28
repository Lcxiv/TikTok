from TikTokApi import TikTokApi
import pandas as pd
api = TikTokApi()


#collect videos from specific user

n_videos = 100
username = 'washingtonpost'
user_videos = api.byUsername(username, count=n_videos)

def simple_dict(tiktok_dict):
  to_return = {}
  to_return['user_name'] = tiktok_dict['author']['uniqueId']
  to_return['user_id'] = tiktok_dict['author']['id']
  to_return['video_id'] = tiktok_dict['id']
  to_return['video_desc'] = tiktok_dict['desc']
  to_return['video_time'] = tiktok_dict['createTime']
  to_return['video_length'] = tiktok_dict['video']['duration']
  to_return['video_link'] = 'https://www.tiktok.com/@{}/video/{}?lang=en'.format(to_return['user_name'], to_return['video_id'])
  to_return['n_likes'] = tiktok_dict['stats']['diggCount']
  to_return['n_shares'] = tiktok_dict['stats']['shareCount']
  to_return['n_comments'] = tiktok_dict['stats']['commentCount']
  to_return['n_plays'] = tiktok_dict['stats']['playCount']
  return to_return

user_videos = [simple_dict(v) for v in user_videos]
user_videos_df = pd.DataFrame(user_videos)
user_videos_df.to_csv('{}_videos.csv'.format(username),index=False)

#videos liked by specific user

username = 'tiktok'
n_videos = 10
liked_videos = api.userLikedbyUsername(username, count=n_videos)
liked_videos = [simple_dict(v) for v in liked_videos]
liked_videos_df = pd.DataFrame(liked_videos)
liked_videos_df.to_csv('{}_liked_videos.csv'.format(username), index=False)


#get multiple users
seed_users = ['tiktok', 'washingtonpost', 'charlidamelio', 'chunkysdead']
seed_ids = [api.getUser(user_name)['userInfo']['user']['id'] for user_name in seed_users]
suggested = [api.getSuggestedUsersbyID(count=20, startingId=s_id) for s_id in seed_ids]


#same thing but random trending accounts

tiktok_id = api.getUser('tiktok')['userInfo']['user']['id']
suggested_100 = api.getSuggestedUsersbyIDCrawler(count=100, startingId=tiktok_id)

#collect trending videos


def main():
    n_trending = 10000
    trending_videos = api.trending(count=n_trending)
    trending_videos = [simple_dict(v) for v in trending_videos]
    trending_videos_df = pd.DataFrame(trending_videos)
    trending_videos_df.to_csv('trending_vids.csv',index=False)

main()
